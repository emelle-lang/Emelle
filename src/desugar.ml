open Base

type 'cmp t =
  { mutable vargen : int
  ; structure : Module.t
  ; symtable : Symtable.t }

(* Tag pattern with register to store its result *)
type 'a pattern = 'a pattern' * int
and 'a pattern' =
  | Ann of 'a * 'a pattern
  | As of 'a pattern * string
  | Con of Ident.t * int * 'a pattern list
  | Wild

let create structure symtable =
  { vargen = 0
  ; structure
  ; symtable }

let fresh_register st =
  st.vargen <- st.vargen + 1;
  st.vargen - 1

(** [pattern_of_ast_pattern state map reg ast_pat]

    Convert [ast_pat] from the AST representation to the representation defined
    in this module, collecting bound identifiers in [map] and returning errors
    when constructors or types aren't defined. *)
let rec pattern_of_ast_pattern st map reg (ann, node) =
  let open Result.Monad_infix in
  let result =
    match node with
    | Ast.Con(constr_path, pats) ->
       let f next acc =
         acc >>= fun (pats, map) ->
         let reg = fresh_register st in
         pattern_of_ast_pattern st map reg next >>| fun (pat, map) ->
         (pat::pats, map)
       in
       begin match
         Module.resolve_path Module.find_constr st.structure constr_path
       with
       | None -> Error (Sequence.return (Message.Unresolved_path constr_path))
       | Some (typename, idx) ->
          List.fold_right ~f:f ~init:(Ok ([], map)) pats >>| fun (pats, map) ->
          ((Con(typename, idx, pats), reg), map)
       end
    | Ast.Var name ->
       begin match Map.add map ~key:name ~data:reg with
       | `Ok map -> Ok ((As((Wild, reg), name), reg), map)
       | `Duplicate -> Error (Sequence.return (Message.Redefined_name name))
       end
    | Ast.Wild -> Ok ((Wild, reg), map)
  in result >>| fun (pat, map) -> ((Ann(ann, pat), reg), map)

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
     Term.Ann { ann = ann; term = term_of_pattern st def cont pat }
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

let rec term_of_expr st env (ann, node) =
  let open Result.Monad_infix in
  let term =
    match node with
    | Ast.App(f, x) ->
       begin match term_of_expr st env f, term_of_expr st env x with
       | Ok f, Ok x -> Ok (Term.App(f, x))
       | (Error e, Ok _) | (Ok _, Error e) -> Error e
       | Error e1, Error e2 -> Error (Sequence.append e1 e2)
       end

    | Ast.Case(test, cases) ->
       term_of_expr st env test >>= fun test ->
       (* The register to store the test in *)
       let reg = fresh_register st in
       let reg_var = Term.Var reg in
       let result =
         List.fold_right
           ~f:(fun (pat, expr) acc ->
             acc >>= fun (i, pats, terms) ->
             let map = Map.empty (module String) in
             (* The register to store the pattern match result in *)
             let reg = fresh_register st in
             pattern_of_ast_pattern st map reg pat >>= fun (pat, map) ->
             Env.in_scope_with (fun env ->
                 term_of_expr st env expr >>| fun body ->
                 let pat_term = term_of_pattern st reg_var body pat in
                 let pat = pattern_t_of_pattern pat in
                 ( (i + 1)
                 , Pattern.{ first_pattern = pat
                           ; rest_patterns = []
                           ; action = i }::pats
                 , pat_term::terms )
               ) map env)
           ~init:(Ok (0, [], []))
           cases
       in
       result >>= fun (_, rows, branches) ->
       let branches = Array.of_list branches in
       Pattern.decision_tree_of_matrix st.symtable rows >>| fun tree ->
       Term.Let(reg, test, (Term.Case([reg_var], tree, branches)))

    | Ast.Lam((_, patterns, _) as case, cases) ->
       let reg = fresh_register st in
       let regs = List.map ~f:(fun _ -> fresh_register st) patterns in
       let handle_branch idx (pat, pats, expr) =
         let map = Map.empty (module String) in
         let tmp_reg = fresh_register st in
         pattern_of_ast_pattern st map tmp_reg pat >>= fun (pat, map) ->
         List.fold_right ~f:(fun pat acc ->
             acc >>= fun (list, map) ->
             let reg = fresh_register st in
             pattern_of_ast_pattern st map reg pat >>| fun (pat, map) ->
             (pat::list, map)
           ) ~init:(Ok ([], map)) pats >>= fun (pats, map) ->
         let row =
           Pattern.{ first_pattern = pattern_t_of_pattern pat
                   ; rest_patterns = List.map ~f:pattern_t_of_pattern pats
                   ; action = idx }
         in
         Env.in_scope_with (fun env ->
             let term =
               let rec f regs patterns =
                 match (regs, patterns) with
                 | [], [] -> term_of_expr st env expr
                 | ([], _::_) | (_::_, []) ->
                    Error (Sequence.return Message.Mismatched_arity)
                 | (reg::regs, pat::pats) ->
                    f regs pats >>| fun cont ->
                    term_of_pattern st (Term.Var reg) cont pat
               in f (reg::regs) (pat::pats)
             in term >>| fun term -> (row, term)
           ) map env
       in
       let branches_result =
         List.fold_right ~f:(fun branch acc ->
             acc >>= fun (i, rows, terms) ->
             handle_branch i branch >>| fun (row, term) ->
             (i + 1, row::rows, term::terms)
           ) ~init:(Ok (0, [], [])) (case::cases)
       in
       branches_result >>= fun (_, matrix, branches) ->
       Pattern.decision_tree_of_matrix st.symtable matrix >>| fun tree ->
       let case_term =
         Term.Case( List.map ~f:(fun reg -> Term.Var reg) (reg::regs)
                  , tree
                  , Array.of_list branches )
       in
       let rec f cont = function
         | [] -> cont
         | (reg::regs) -> Term.Lam(reg, f cont regs)
       in f case_term (reg::regs)

    | Ast.Let(bindings, body) ->
       let rec f map = function
         | [] -> Env.in_scope_with (fun env -> term_of_expr st env body) map env
         | (pat, def)::xs ->
            term_of_expr st env def >>= fun def ->
            let reg = fresh_register st in
            pattern_of_ast_pattern st map reg pat >>= fun (pat, map) ->
            let pat' = pattern_t_of_pattern pat in
            let matrix = [ Pattern.{ first_pattern = pat'
                                   ; rest_patterns = []
                                   ; action = 0 } ]
            in
            Pattern.decision_tree_of_matrix st.symtable matrix >>= fun tree ->
            f map xs >>| fun cont ->
            let term = term_of_pattern st def cont pat in
            Term.Case([def], tree, [|term|])
       in f (Map.empty (module String)) bindings

    | Ast.Let_rec(bindings, body) ->
       let bindings =
         List.fold_right ~f:(fun (str, expr) acc ->
             acc >>= fun (list, env) ->
             let reg = fresh_register st in
             match Env.add env str reg with
             | Some env -> Ok ((reg, expr)::list, env)
             | None ->
                Error (Sequence.return (Message.Redefined_name str))
           ) ~init:(Ok ([], env)) bindings;
       in
       bindings >>= fun (bindings, env) ->
       Env.in_scope (fun env ->
           let bindings =
             List.fold_right ~f:(fun (reg, expr) acc ->
                 acc >>= fun list ->
                 term_of_expr st env expr >>| fun term ->
                 (reg, term)::list
               ) ~init:(Ok []) bindings
           in
           bindings >>= fun bindings ->
           term_of_expr st env body >>| fun body ->
           Term.Let_rec(bindings, body)
         ) env

    | Ast.Var ((prefix, name) as path) ->
       match prefix with
       | [] -> (* Unqualified name *)
          begin match Env.find env name with
          (* Found in the local environment *)
          | Some reg -> Ok (Term.Var reg)
          | None ->
             (* Search in the current structure *)
             match Module.find_val st.structure name with
             | Some ident -> Ok (Term.Extern_var ident)
             | None -> Error (Sequence.return (Message.Unresolved_path path))
          end
       | _ -> (* Qualified name *)
          match Module.resolve_path Module.find_val st.structure path with
          | None -> Ok (Term.Extern_var (Ident.of_path path))
          | Some ident -> Ok (Term.Extern_var ident)

  in term >>| fun term -> (Term.Ann { ann = ann; term = term })
