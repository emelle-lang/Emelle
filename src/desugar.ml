open Base

type t = {
    mutable vargen : int;
    mutable registers : (string, int) Env.t;
    typedefs : (Ident.t, Type.def) Env.t;
  }

(* Tag pattern with register *)
type 'a pattern = 'a pattern' * int
and 'a pattern' =
  | Ann of 'a * 'a pattern
  | As of 'a pattern * string
  | Con of Ident.t * int * 'a pattern list
  | Wild

let fresh_register st =
  st.vargen <- st.vargen + 1;
  st.vargen - 1

let with_registers f st tbl =
  let old = st.registers in
  st.registers <- Env.extend tbl st.registers;
  let result = f st in
  st.registers <- old;
  result

let idx_of_constr st typename con =
  match Env.find st.typedefs typename with
  | Some (Algebraic adt) ->
     begin match Hashtbl.find adt.constr_names con with
     | Some idx -> Ok idx
     | None -> Error (Sequence.return (Message.Unknown_constr(typename, con)))
     end
  | Some (Alias _) -> Error (Sequence.return (Message.Expected_adt typename))
  | None -> Error (Sequence.return (Message.Unresolved_type typename))

(** Convert a pattern from the AST representation to the representation defined
    in this module, returning errors when constructors or types aren't defined.

    This function is parameterized over Hashtbl.t, the hash table to store the
    string-to-register mappings in, instead of using the state's [registers]
    member, because there are some cases in which the register definitions
    should be stored in another hashtbl. *)
let rec pattern_of_ast_pattern st hashtbl reg (ann, node) =
  let open Result.Monad_infix in
  let pat = match node with
    | Ast.Con(typename, con, pats) ->
       let f next acc =
         acc >>= fun pats ->
         let reg = fresh_register st in
         pattern_of_ast_pattern st hashtbl reg next >>| fun pat ->
         pat::pats
       in
       idx_of_constr st typename con >>= fun idx ->
       List.fold_right ~f:f ~init:(Ok []) pats >>| fun patterns ->
       (Con(typename, idx, patterns), reg)
    | Ast.Var id ->
       begin match Hashtbl.add ~key:id ~data:reg hashtbl with
       | `Ok -> Ok (As((Wild, reg), id), reg)
       | `Duplicate ->
          Error (Sequence.return (Message.Redefined_id (Ident.Local id)))
       end
    | Ast.Wild -> Ok (Wild, reg)
  in pat >>| fun pat -> (Ann(ann, pat), reg)

(** Compile the pattern from the representation defined in this module to the
    one defined in the Pattern module *)
let rec pattern_t_of_pattern (pat, _) =
  match pat with
  | Ann(_, pat) | As(pat, _) -> pattern_t_of_pattern pat
  | Con(typename, constr_idx, pats) ->
     Pattern.Con(typename, constr_idx, List.map ~f:pattern_t_of_pattern pats)
  | Wild -> Pattern.Wild

(** Walk the pattern and compile it into a term that assumes that all pattern
    matches are successful  *)
let rec term_of_pattern st def cont (node, reg) =
  match node with
  | Ann(ann, pat) ->
     Term.Ann {ann = ann; term = term_of_pattern st def cont pat}
  | As(pat, _) -> term_of_pattern st def cont pat
  | Con(typename, constr_idx, pats) ->
     let rec f i = function
       | [] -> cont
       | (pat::pats) ->
          term_of_pattern st (
              Term.Select(typename, constr_idx, i, Term.Var reg)
            ) (f (i + 1) pats) pat
     in f 0 pats
  | Wild -> Term.Let(reg, def, cont)

let rec term_of_expr st (ann, node) =
  let open Result.Monad_infix in
  let term =
    match node with
    | Ast.App(f, x) ->
       begin match term_of_expr st f, term_of_expr st x with
       | Ok f, Ok x -> Ok (Term.App(f, x))
       | (Error e, Ok _) | (Ok _, Error e) -> Error e
       | Error e1, Error e2 -> Error (Sequence.append e1 e2)
       end

    | Ast.Case(test, cases) ->
       term_of_expr st test >>= fun test ->
       let reg = fresh_register st in
       let reg_var = Term.Var reg in
       let result =
         List.fold_right
           ~f:(fun (pat, expr) acc ->
             acc >>= fun (i, pats, terms) ->
             with_registers (fun _ ->
                 let reg = fresh_register st in
                 let hashtbl = Env.table st.registers in
                 pattern_of_ast_pattern st hashtbl reg pat >>= fun pat ->
                 term_of_expr st expr >>| fun body ->
                 let pat_term = term_of_pattern st reg_var body pat in
                 let pat = pattern_t_of_pattern pat in
                 ( (i + 1)
                 , Pattern.{ first_pattern = pat
                           ; rest_patterns = []
                           ; action = i }::pats
                 , pat_term::terms )
               ) st (Hashtbl.create (module String)))
           ~init:(Ok (0, [], []))
           cases
       in
       result >>= fun (_, rows, branches) ->
       let branches = Array.of_list branches in
       begin match Pattern.decision_tree_of_matrix st.typedefs rows with
       | Some tree ->
          Ok (Term.Let(reg, test, (Term.Case([reg_var], tree, branches))))
       | None -> Error (Sequence.return (Message.Unreachable))
       end

    | Ast.Lam((_, patterns, _) as case, cases) ->
       let reg = fresh_register st in
       let regs = List.map ~f:(fun _ -> fresh_register st) patterns in
       let handle_branch idx (pattern, patterns, expr) =
         with_registers (fun st ->
             let temp_reg = fresh_register st in
             let tbl = Env.table st.registers in
             pattern_of_ast_pattern st tbl temp_reg pattern >>= fun pattern ->
             List.fold_right ~f:(fun pattern acc ->
                 acc >>= fun list ->
                 let reg = fresh_register st in
                 pattern_of_ast_pattern st tbl reg pattern >>| fun pattern ->
                 pattern::list
               ) ~init:(Ok []) patterns >>= fun patterns ->
             let row =
               Pattern.{ first_pattern = pattern_t_of_pattern pattern
                       ; rest_patterns =
                           List.map ~f:pattern_t_of_pattern patterns
                       ; action = idx }
             in
             let term =
               let rec f regs patterns =
                 match (regs, patterns) with
                 | [], [] -> term_of_expr st expr
                 | ([], _::_) | (_::_, []) ->
                    Error (Sequence.return Message.Mismatched_arity)
                 | (reg::regs, pat::pats) ->
                    f regs pats >>| fun cont ->
                    term_of_pattern st (Term.Var reg) cont pat
               in f (reg::regs) (pattern::patterns)
             in term >>| fun term -> (row, term)
           ) st (Hashtbl.create (module String))
       in
       let branches_result =
         List.fold_right ~f:(fun branch acc ->
             acc >>= fun (i, rows, terms) ->
             handle_branch i branch >>| fun (row, term) ->
             (i + 1, row::rows, term::terms)
           ) ~init:(Ok (0, [], [])) (case::cases)
       in
       branches_result >>= fun (_, matrix, branches) ->
       begin match Pattern.decision_tree_of_matrix st.typedefs matrix with
       | Some tree ->
          let case_term =
            Term.Case( List.map ~f:(fun reg -> Term.Var reg) (reg::regs)
                     , tree
                     , Array.of_list branches )
          in
          let rec f cont = function
            | [] -> cont
            | (reg::regs) -> Term.Lam(reg, f cont regs)
          in Ok (f case_term (reg::regs))
       | None -> Error (Sequence.return Message.Unreachable)
       end

    | Ast.Let(bindings, body) ->
       let new_bindings = Hashtbl.create (module String) in
       let rec f = function
         | [] -> with_registers (fun _ -> term_of_expr st body) st new_bindings
         | (pat, def)::xs ->
            term_of_expr st def >>= fun def ->
            let reg = fresh_register st in
            pattern_of_ast_pattern st new_bindings reg pat >>= fun pat ->
            let pat' = pattern_t_of_pattern pat in
            let matrix = [ Pattern.{ first_pattern = pat'
                                   ; rest_patterns = []
                                   ; action = 0 } ]
            in
            match Pattern.decision_tree_of_matrix st.typedefs matrix with
            | Some tree ->
               f xs >>| fun cont ->
               let term = term_of_pattern st def cont pat in
               Term.Case([def], tree, [|term|])
            | None -> Error (Sequence.return Message.Unreachable)
       in f bindings

    | Ast.Let_rec(bindings, body) ->
       let hashtbl = Hashtbl.create (module String) in
       let bindings =
         List.map ~f:(fun (str, expr) ->
             let reg = fresh_register st in
             Hashtbl.add_exn hashtbl ~key:str ~data:reg;
             (reg, expr)
           ) bindings;
       in
       with_registers (fun st ->
           let bindings =
             List.fold_right ~f:(fun (reg, expr) acc ->
                 acc >>= fun list ->
                 term_of_expr st expr >>| fun term ->
                 (reg, term)::list
               ) ~init:(Ok []) bindings
           in
           bindings >>= fun bindings ->
           term_of_expr st body >>| fun body ->
           Term.Let_rec(bindings, body)
         ) st hashtbl

    | Ast.Var id ->
       begin match id with
       | Local str ->
          begin match Env.find st.registers str with
          | Some reg -> Ok (Term.Var reg)
          | None -> Error (Sequence.return (Message.Unresolved_id id))
          end
       | Path _ -> Error (Sequence.return (Message.Unimplemented "path"))
       end

  in term >>| fun term -> (Term.Ann { ann = ann; term = term })
