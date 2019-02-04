open Base

type t =
  { vargen : Ident.gen
  ; packages : (string, Package.t) Hashtbl.t
  ; package : Package.t }

(** Create a fresh desugarer state *)
let create package packages =
  { vargen = Ident.create_gen ()
  ; package
  ; packages }

let find f st = function
  | Ast.Internal str ->
     begin match f st.package str with
     | None -> None
     | Some x -> Some ((st.package.Package.name, str), x)
     end
  | Ast.External(pack_name, item_name) ->
     match Hashtbl.find st.packages pack_name with
     | None -> None
     | Some package ->
        match f package item_name with
        | None -> None
        | Some x -> Some ((pack_name, item_name), x)

let fresh_ident st name = Ident.fresh st.vargen name

(** [pattern_of_ast_pattern state map reg ast_pat] converts [ast_pat] from an
    [Ast.pattern] to [Term.ml] while collecting bound identifiers in [map],
    returning [Error] if a data constructor or type isn't defined. *)
let rec pattern_of_ast_pattern st map id_opt ast_pat =
  let open Result.Monad_infix in
  match ast_pat.Ast.pat_node with
  | Ast.Con(constr_path, pats) ->
     let f next acc =
       acc >>= fun (pats, map) ->
       pattern_of_ast_pattern st map None next >>| fun (pat, map) ->
       (pat::pats, map)
     in
     begin match find Package.find_adt st constr_path with
     | None -> Error (Sequence.return (Message.Unresolved_path constr_path))
     | Some (_, (adt, idx)) ->
        List.fold_right ~f:f ~init:(Ok ([], map)) pats >>| fun (pats, map) ->
        ( { Pattern.ann = ast_pat.Ast.pat_ann
          ; node = Con(adt, idx, pats)
          ; id = id_opt }
        , map)
     end
  | Ast.Deref pat ->
     pattern_of_ast_pattern st map None pat >>| fun (pat, map) ->
     ( { Pattern.ann = ast_pat.Ast.pat_ann
       ; node = Deref pat
       ; id = id_opt }
     , map)
  | Ast.Unit ->
     Ok ( { Pattern.ann = ast_pat.Ast.pat_ann
          ; node = Unit
          ; id = id_opt }
        , map )
  | Ast.Var name ->
     let id =
       match id_opt with
       | Some id -> id
       | None -> fresh_ident st (Some name)
     in
     begin match Map.add map ~key:name ~data:id with
     | `Ok map ->
        Ok ( { Pattern.ann = ast_pat.Ast.pat_ann
             ; node = Wild
             ; id = Some id }
           , map )
     | `Duplicate -> Error (Sequence.return (Message.Redefined_name name))
     end
  | Ast.Wild ->
     Ok ({ Pattern.ann = ast_pat.Ast.pat_ann; node = Wild; id = id_opt }, map)

(** [term_of_expr desugarer env expr] converts [expr] from an [Ast.expr] to a
    [Term.t]. *)
let rec term_of_expr st env { Ast.expr_ann = ann; expr_node = node } =
  let open Result.Monad_infix in
  let term =
    match node with
    | Ast.App(f, x) ->
       begin match term_of_expr st env f, term_of_expr st env x with
       | Ok f, Ok x -> Ok (Term.App(f, x))
       | (Error e, Ok _) | (Ok _, Error e) -> Error e
       | Error e1, Error e2 -> Error (Sequence.append e1 e2)
       end

    | Ast.Assign(lval, rval) ->
       term_of_expr st env lval >>= fun lval ->
       term_of_expr st env rval >>| fun rval ->
       Term.Assign(lval, rval)

    | Ast.Case(scrutinee, cases) ->
       term_of_expr st env scrutinee >>= fun scrutinee ->
       List.fold_right ~f:(fun (pat, expr) acc ->
           acc >>= fun cases ->
           let map = Map.empty (module String) in
           pattern_of_ast_pattern st map None pat >>= fun (pat, map) ->
           Env.in_scope_with (fun env ->
               term_of_expr st env expr
             ) map env
           >>| fun body ->
           let ids =
             Map.fold ~f:(fun ~key:_ ~data:id set ->
                 Set.add set id
               ) ~init:(Set.empty (module Ident)) map
           in
           ([pat], ids, body)::cases
         ) ~init:(Ok []) cases >>| fun cases ->
       Term.Case([scrutinee], cases)

    | Ast.Constr path ->
       begin match path with
       | Ast.Internal name ->
          begin match Package.find_adt st.package name with
          | Some (adt, idx) -> Ok (Term.Constr(adt, idx))
          | None -> Error (Sequence.return (Message.Unresolved_path path))
          end
       | Ast.External _ ->
          match find Package.find_adt st path with
          | Some (_, (adt, idx)) -> Ok (Term.Constr(adt, idx))
          | None -> Error (Sequence.return (Message.Unresolved_path path))
       end

    | Ast.Lam((_, patterns, _) as case, cases) ->
       let id = fresh_ident st None in
       let ids = List.map ~f:(fun _ -> fresh_ident st None) patterns in
       let handle_branch (pat, pats, expr) =
         let map = Map.empty (module String) in
         pattern_of_ast_pattern st map None pat >>= fun (pat, map) ->
         List.fold_right ~f:(fun pat acc ->
             acc >>= fun (list, map) ->
             pattern_of_ast_pattern st map None pat >>| fun (pat, map) ->
             (pat::list, map)
           ) ~init:(Ok ([], map)) pats >>= fun (pats, map) ->
         Env.in_scope_with (fun env ->
             term_of_expr st env expr
           ) map env >>| fun term ->
         let ids =
           Map.fold ~f:(fun ~key:_ ~data:id acc ->
               Set.add acc id
             ) ~init:(Set.empty (module Ident)) map
         in (pat::pats, ids, term)
       in
       List.fold_right ~f:(fun branch acc ->
           acc >>= fun rows ->
           handle_branch branch >>| fun row ->
           row::rows
         ) ~init:(Ok []) (case::cases) >>| fun cases ->
       let case_term =
         { Term.ann
         ; term =
             let f x = { Term.ann; term = Term.Var x} in
             Case(List.map ~f (id::ids), cases) }
       in
       List.fold_right ~f:(fun id body ->
           { Term.ann; term = Lam(id, body) }
         ) ~init:case_term ids
       |> fun body -> Term.Lam(id, body)

    | Ast.Let(bindings, body) ->
       (* Transform

              let p1 = e1
              and p2 = e2
              ... pN = eN
              in body

          into

              case e1, e2, ... eN with
              | p1, p2, ... pN -> body

          What should the semantics be regarding diverging RHS expressions and
          refutable patterns? One would expect the let bindings' RHS to
          evaluate and match with its LHS pattern from top to bottom, but with
          the case desugar, all of the RHS expressions evaluate before any of
          them match with the LHS pattern. Is this desugar sensible? *)
       elab_let_bindings st env bindings
       >>= fun (map, scruts, ids, pats) ->
       Env.in_scope_with (fun env -> term_of_expr st env body) map env
       >>| fun body -> Term.Case(scruts, [pats, ids, body])

    | Ast.Let_rec(bindings, body) ->
       Env.in_scope (fun env ->
           elab_rec_bindings st env bindings >>= fun (env, bindings) ->
           term_of_expr st env body >>| fun body ->
           Term.Let_rec(bindings, body)
         ) env

    | Ast.Lit lit -> Ok (Term.Lit lit)

    | Ast.Prim(op, ty) -> Ok (Term.Prim(op, ty))

    | Ast.Ref -> Ok Term.Ref

    | Ast.Seq(s, t) ->
       term_of_expr st env s >>= fun s ->
       term_of_expr st env t >>| fun t ->
       Term.Seq(s, t)

    | Ast.Var qual_id ->
       match qual_id with
       | Ast.Internal name -> (* Unqualified name *)
          begin match Env.find env name with
          (* Found in the local environment *)
          | Some id -> Ok (Term.Var id)
          | None -> Error (Sequence.return (Message.Unresolved_path qual_id))
          end
       | Ast.External _ -> (* Qualified name *)
          match find Package.find_val st qual_id with
          | Some (path, (ty, _)) -> Ok (Term.Extern_var(path, ty))
          | None -> Error (Sequence.return (Message.Unresolved_path qual_id))

  in term >>| fun term -> { Term.ann = ann; term = term }

and elab_rec_bindings self env bindings =
  let open Result.Monad_infix in
  List.fold_right ~f:(fun (str, expr) acc ->
      acc >>= fun (env, list) ->
      let id = fresh_ident self (Some str) in
      match Env.add env str id with
      | Some env -> Ok (env, (id, expr)::list)
      | None -> Error (Sequence.return (Message.Redefined_name str))
    ) ~init:(Ok (env, [])) bindings
  >>= fun (env, bindings) ->
  List.fold_right ~f:(fun (id, expr) acc ->
      acc >>= fun list ->
      term_of_expr self env expr >>| fun term ->
      (id, term)::list
    ) ~init:(Ok []) bindings
  >>| fun bindings -> (env, bindings)

and elab_let_bindings self env bindings =
  let open Result.Monad_infix in
  let helper map (pat, expr) =
    pattern_of_ast_pattern self map None pat
    >>= fun (pat, map) ->
    term_of_expr self env expr
    >>| fun term ->
    (pat, term, map)
  in
  let map = Map.empty (module String) in
  List.fold_right ~f:(fun binding acc ->
      acc >>= fun (map, scruts, pats) ->
      helper map binding >>| fun (pat, term, map) ->
      (map, term::scruts, pat::pats)
    ) ~init:(Ok (map, [], [])) bindings
  >>| fun (map, scruts, pats) ->
  let ids =
    Map.fold ~f:(fun ~key:_ ~data:id acc ->
        Set.add acc id
      ) ~init:(Set.empty (module Ident)) map
  in (map, scruts, ids, pats)

let elab typechecker env package packages ast_file =
  let open Result.Monad_infix in
  let elab = create package packages in
  List.fold ast_file.Ast.file_items ~init:(Ok (env, [])) ~f:(fun acc next ->
      acc >>= fun (env, list) ->
      match next with
      | Ast.Let bindings ->
         elab_let_bindings elab env bindings
         >>= fun (map, scruts, ids, pats) ->
         Map.fold ~f:(fun ~key:key ~data:data acc ->
             acc >>= fun env ->
             match Env.add env key data with
             | Some env -> Ok env
             | None -> Error (Sequence.return (Message.Redefined_name key))
           ) ~init:(Ok env) map
         >>| fun env ->
         (env, (Term.Top_let(scruts, ids, pats))::list)
      | Ast.Let_rec bindings ->
         elab_rec_bindings elab env bindings
         >>| fun (env, bindings) ->
         (env, (Term.Top_let_rec bindings)::list)
      | Ast.Type(adt, adts) ->
         List.fold ~f:(fun acc adt ->
             acc >>= fun () ->
             let kvar = Kind.fresh_var typechecker.Typecheck.kvargen in
             Package.add_typedef
               package adt.Ast.adt_name (Package.Todo (Kind.Var kvar))
           ) ~init:(Ok ()) (adt::adts) >>= fun () ->
         List.fold ~f:(fun acc adt ->
             acc >>= fun () ->
             Typecheck.type_adt_of_ast_adt typechecker adt >>= fun adt ->
             match Package.find_typedef package adt.Type.name with
             | None ->
                Error (Sequence.return (Message.Unreachable "Typecheck ADT"))
             | Some ptr ->
                match !ptr with
                | Package.Compiled _ ->
                   Error (Sequence.return
                            (Message.Redefined_name adt.Type.name))
                | Package.Todo kind ->
                   Typecheck.unify_kinds kind (Type.kind_of_adt adt)
                   >>= fun () ->
                   Package.add_constrs package adt >>= fun () ->
                   ptr := Package.Compiled (Type.Manifest adt);
                   Ok ()
                | Package.Prim _ ->
                   Package.add_constrs package adt >>= fun () ->
                   ptr := Package.Compiled (Type.Manifest adt);
                   Ok ()
           ) ~init:(Ok ()) (adt::adts) >>| fun () ->
         (env, list)
    )
  >>| fun (env, list) ->
  { Term.top_ann = ast_file.Ast.file_ann
  ; exports = ast_file.Ast.file_exports
  ; env = env
  ; items = List.rev list }
