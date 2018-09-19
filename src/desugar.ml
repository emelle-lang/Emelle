open Base

type 'cmp t =
  { mutable vargen : int
  ; structure : Module.t
  ; symtable : Symtable.t }

let create structure symtable =
  { vargen = 0
  ; structure
  ; symtable }

let fresh_register st =
  st.vargen <- st.vargen + 1;
  st.vargen - 1

(** [pattern_of_ast_pattern state map reg ast_pat]

    Convert [ast_pat] from the AST representation to the representation defined
    in pattern.ml, collecting bound identifiers in [map] and returning errors
    when constructors or types aren't defined. *)
let rec pattern_of_ast_pattern st map reg (_, node) =
  let open Result.Monad_infix in
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
        (Pattern.{node = Pattern.Con(typename, idx, pats); reg = Some reg}, map)
     end
  | Ast.Var name ->
     begin match Map.add map ~key:name ~data:reg with
     | `Ok map -> Ok (Pattern.{node = Pattern.Wild; reg = Some reg}, map)
     | `Duplicate -> Error (Sequence.return (Message.Redefined_name name))
     end
  | Ast.Wild -> Ok (Pattern.{node = Pattern.Wild; reg = Some reg}, map)

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
       List.fold_right
         ~f:(fun (pat, expr) acc ->
           acc >>= fun pats ->
           let map = Map.empty (module String) in
           (* The register to store the pattern match result in *)
           let reg = fresh_register st in
           pattern_of_ast_pattern st map reg pat >>= fun (pat, map) ->
           Env.in_scope_with (fun env ->
               term_of_expr st env expr >>| fun body ->
               Pattern.{ first_pattern = pat
                       ; rest_patterns = []
                       ; action = body }::pats
             ) map env)
         ~init:(Ok [])
         cases >>= fun rows ->
       Pattern.decision_tree_of_matrix [[]] st.symtable rows >>| fun tree ->
       Term.Case([test], tree)

    | Ast.Lam((_, patterns, _) as case, cases) ->
       let reg = fresh_register st in
       let regs = List.map ~f:(fun _ -> fresh_register st) patterns in
       let handle_branch (pat, pats, expr) =
         let map = Map.empty (module String) in
         let tmp_reg = fresh_register st in
         pattern_of_ast_pattern st map tmp_reg pat >>= fun (pat, map) ->
         List.fold_right ~f:(fun pat acc ->
             acc >>= fun (list, map) ->
             let reg = fresh_register st in
             pattern_of_ast_pattern st map reg pat >>| fun (pat, map) ->
             (pat::list, map)
           ) ~init:(Ok ([], map)) pats
         >>= fun (pats, map) ->
         Env.in_scope_with (fun env ->
             term_of_expr st env expr
           ) map env
         >>| fun term ->
         Pattern.{ first_pattern = pat
                 ; rest_patterns = pats
                 ; action = term }
       in
       List.fold_right ~f:(fun branch acc ->
           acc >>= fun rows ->
           handle_branch branch >>| fun row ->
           row::rows
         ) ~init:(Ok []) (case::cases) >>= fun matrix ->
       Pattern.decision_tree_of_matrix [[]] st.symtable matrix >>| fun tree ->
       let case_term =
         Term.Case(List.map ~f:(fun reg -> Term.Var reg) (reg::regs), tree)
       in
       List.fold_right ~f:(fun reg body -> Term.Lam(reg, body)
         ) ~init:case_term (reg::regs)

    | Ast.Let(bindings, body) ->
       let rec f map = function
         | [] -> Env.in_scope_with (fun env -> term_of_expr st env body) map env
         | (pat, def)::bindings ->
            term_of_expr st env def >>= fun def ->
            let reg = fresh_register st in
            pattern_of_ast_pattern st map reg pat >>= fun (pat, map) ->
            f map bindings >>= fun cont ->
            let matrix = [ Pattern.{ first_pattern = pat
                                   ; rest_patterns = []
                                   ; action = cont } ]
            in
            Pattern.decision_tree_of_matrix [[]] st.symtable matrix
            >>| fun tree ->
            Term.Case([def], tree)
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
