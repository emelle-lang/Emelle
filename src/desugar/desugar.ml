(* Copyright (C) 2018-2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base

type t =
  { vargen : Ident.gen
  ; imports : (string, Package.t) Hashtbl.t
  ; packages : (Qual_id.Prefix.t, Package.t) Hashtbl.t
  ; package : Package.t }

let create package packages =
  { vargen = Ident.create_gen ()
  ; imports = Hashtbl.create (module String)
  ; package
  ; packages }

let find f error st = function
  | Ast.Internal name ->
     begin match f st.package name with
     | None -> Error (Message.Unresolved_name name)
     | Some x -> Ok ({ Qual_id.prefix = st.package.Package.prefix; name }, x)
     end
  | Ast.External(mod_alias, name) ->
     match Hashtbl.find st.imports mod_alias with
     | None -> Error (Message.Unresolved_name mod_alias)
     | Some package ->
        match f package name with
        | None -> Error (error name)
        | Some x -> Ok ({ Qual_id.prefix = package.Package.prefix; name }, x)

let fresh_ident st name = Ident.fresh st.vargen name

(** [pattern_of_ast_pattern state map reg ast_pat] converts [ast_pat] from an
    [Ast.pattern] to [Term.ml] while collecting bound identifiers in [map],
    returning [Error] if a data constructor or type isn't defined. *)
let rec pattern_of_ast_pattern st map id_opt ast_pat =
  let open Result.Let_syntax in
  match ast_pat.Ast.pat_node with
  | Ast.Con(constr_path, pats) ->
     let f next acc =
       acc >>= fun (pats, map) ->
       pattern_of_ast_pattern st map None next >>| fun (pat, map) ->
       (pat :: pats, map)
     in
     begin match
       find Package.find_adt
         (fun name -> Message.Unresolved_name name) st constr_path
     with
     | Error e -> Message.error ast_pat.Ast.pat_ann e
     | Ok (_, (adt, idx)) ->
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
     | `Duplicate ->
        Message.error ast_pat.Ast.pat_ann (Message.Redefined_name name)
     end
  | Ast.Wild ->
     Ok ({ Pattern.ann = ast_pat.Ast.pat_ann; node = Wild; id = id_opt }, map)

(** Convert an Ast.monotype into an Type.t *)
let rec normalize t tvars { Ast.ty_node = node; Ast.ty_ann = ann } =
  let open Result.Monad_infix in
  match node with
  | Ast.TApp(constr, arg) ->
     normalize t tvars constr >>= fun constr ->
     normalize t tvars arg >>| fun arg ->
     Type.App(constr, arg)
  | Ast.TApplied_arrow(dom, codom) ->
     normalize t tvars dom >>= fun dom ->
     normalize t tvars codom >>| fun codom ->
     Type.arrow dom codom
  | Ast.TArrow -> Ok (Type.Prim Type.Arrow)
  | Ast.TRef -> Ok (Type.Prim Type.Ref)
  | Ast.TNominal path ->
     begin
       match
         find Package.find_typedef
           (fun name -> Message.Unresolved_name name) t path
       with
       | Ok (_, { contents = Package.Prim prim }) -> Ok (Type.Prim prim)
       | Ok (ident, _) -> Ok (Type.Nominal ident)
       | Error e -> Message.error ann e
     end
  | Ast.TVar name ->
     match Env.find tvars name with
     | Some tvar -> Ok tvar
     | None -> Message.error ann (Message.Unresolved_typevar name)

let fresh_kinds_of_typeparams checker =
  List.map ~f:(fun (name, purity) ->
      (Kind.Var (Kind.fresh_var checker.Typecheck.kvargen), name, purity)
    )

let tvars_of_typeparams checker tvar_map decls =
  let open Result.Let_syntax in
  let rec loop tvar_map tvar_list = function
    | (kind, str, purity) :: decls ->
       let tvar =
         match purity with
         | Ast.Pure ->
            Type.fresh_rigid checker.Typecheck.tvargen Type.Pure 0 kind
         | Ast.Impure i ->
            Type.fresh_rigid checker.Typecheck.tvargen Type.Impure i kind
       in
       begin match Env.add tvar_map str (Type.Var (ref (Type.Rigid tvar))) with
       | None -> Error (Message.Redefined_typevar str)
       | Some tvar_map ->
          (* Fold RIGHT, not left! *)
          let%map tvar_map, tvar_list = loop tvar_map tvar_list decls in
          tvar_map, tvar :: tvar_list
       end
    | [] -> Ok (tvar_map, tvar_list)
  in loop tvar_map [] decls

let type_of_ast_polytype
      t checker
      { Ast.polyty_params = typeparams
      ; polyty_body = body
      ; polyty_ann = ann } =
  let open Result.Monad_infix in
  let tvar_map = Env.empty (module String) in
  let typeparams = fresh_kinds_of_typeparams checker typeparams in
  Message.at ann (tvars_of_typeparams checker tvar_map typeparams)
  >>= fun (tvar_map, tvar_list) ->
  normalize t tvar_map body >>| fun body ->
  Type.Forall(tvar_list, body)

let find_var t env ann qual_id =
  match qual_id with
  | Ast.Internal name -> (* Unqualified name *)
     begin match Env.find env name with
     (* Found in the local environment *)
     | Some id -> Ok (Term.Var id)
     | None -> Message.error ann (Message.Unresolved_name name)
     end
  | Ast.External _ -> (* Qualified name *)
     match
       find Package.find_val
         (fun name -> Message.Unresolved_name name) t qual_id
     with
     | Ok ({ Qual_id.prefix; _ }, (ty, offset)) ->
        Ok (Term.Extern_var (prefix, offset, ty))
     | Error e -> Message.error ann e

let rec term_of_expr t checker env { Ast.expr_ann = ann; expr_node = node } =
  let open Result.Let_syntax in
  let%map term =
    match node with
    | Ast.App(f, x) ->
       begin match
         term_of_expr t checker env f, term_of_expr t checker env x
       with
       | Ok f, Ok x -> Ok (Term.App(f, x))
       | (Error e, Ok _) | (Ok _, Error e) -> Error e
       | Error e1, Error e2 -> Error (Message.And(e1, e2))
       end

    | Ast.Assign(lval, rval) ->
       let%bind lval = term_of_expr t checker env lval in
       let%map rval = term_of_expr t checker env rval in
       Term.Assign(lval, rval)

    | Ast.Case(scrutinee, cases) ->
       let%bind scrutinee = term_of_expr t checker env scrutinee in
       let%map cases =
         List.fold_right ~f:(fun (pat, expr) acc ->
             let%bind cases = acc in
             let map = Map.empty (module String) in
             let%bind pat, map = pattern_of_ast_pattern t map None pat in
             let%map body =
               Env.in_scope_with (fun env ->
                   term_of_expr t checker env expr
                 ) map env
             in
             let ids =
               Map.fold ~f:(fun ~key:_ ~data:id set ->
                   Set.add set id
                 ) ~init:(Set.empty (module Ident)) map
             in
             ([pat], ids, body) :: cases
           ) ~init:(Ok []) cases
       in Term.Case([scrutinee], cases)

    | Ast.Constr path ->
       begin match path with
       | Ast.Internal name ->
          begin match Package.find_adt t.package name with
          | Some (adt, idx) -> Ok (Term.Constr(adt, idx))
          | None -> Message.error ann (Message.Unresolved_name name)
          end
       | Ast.External _ ->
          match
            find Package.find_adt
              (fun name -> Message.Unresolved_name name) t path
          with
          | Ok (_, (adt, idx)) -> Ok (Term.Constr(adt, idx))
          | Error e -> Message.error ann e
       end

    | Ast.Lam((_, patterns, _) as case, cases) ->
       let id = fresh_ident t None in
       let ids = List.map ~f:(fun _ -> fresh_ident t None) patterns in
       let handle_branch (pat, pats, expr) =
         let map = Map.empty (module String) in
         pattern_of_ast_pattern t map None pat >>= fun (pat, map) ->
         let%bind pats, map =
           List.fold_right ~f:(fun pat acc ->
               let%bind list, map = acc in
               let%map pat, map = pattern_of_ast_pattern t map None pat in
               (pat :: list, map)
             ) ~init:(Ok ([], map)) pats
         in
         let%map term =
           Env.in_scope_with (fun env ->
               term_of_expr t checker env expr
             ) map env
         in
         let ids =
           Map.fold ~f:(fun ~key:_ ~data:id acc ->
               Set.add acc id
             ) ~init:(Set.empty (module Ident)) map
         in (pat :: pats, ids, term)
       in
       let%map cases =
         List.fold_right ~f:(fun branch acc ->
             let%bind rows = acc in
             let%map row = handle_branch branch in
             row :: rows
           ) ~init:(Ok []) (case :: cases)
       in
       let case_term =
         { Term.ann
         ; term =
             let f x = { Term.ann; term = Term.Var x} in
             Case(List.map ~f (id :: ids), cases) }
       in
       let body =
         List.fold_right ~f:(fun id body ->
             { Term.ann; term = Lam(id, body) }
           ) ~init:case_term ids
       in Term.Lam(id, body)

    | Ast.Let(bindings, body) ->
       (* Transform

              let p1 = e1
              and p2 = e2
              ... pN = eN
              in body

          into

              case e1, e2, ... eN with
              | p1, p2, ... pN -> body *)
       let%bind map, scruts, ids, pats =
         desugar_let_bindings t checker env bindings in
       let%map body =
         Env.in_scope_with (fun env -> term_of_expr t checker env body) map env
       in Term.Case(scruts, [pats, ids, body])

    | Ast.Let_rec(bindings, body) ->
       Env.in_scope (fun env ->
           let%bind env, bindings =
             desugar_rec_bindings t checker env bindings in
           let%map body = term_of_expr t checker env body in
           Term.Let_rec(bindings, body)
         ) env

    | Ast.Lit lit -> Ok (Term.Lit lit)

    | Ast.Op(lhs, op, rhs) ->
       let%bind lhs = term_of_expr t checker env lhs in
       let%bind rhs = term_of_expr t checker env rhs in
       let%map op = find_var t env ann op in
       Term.App
         ( { Term.term = Term.App({ Term.term = op; ann }, lhs); ann }
         , rhs )

    | Ast.Prim(op, ty) ->
       type_of_ast_polytype t checker ty >>| fun ty ->
       Term.Prim(op, ty)

    | Ast.Ref -> Ok Term.Ref

    | Ast.Seq(e1, e2) ->
       let%bind e1 = term_of_expr t checker env e1 in
       let%map e2 = term_of_expr t checker env e2 in
       Term.Seq(e1, e2)

    | Ast.Typed_hole -> Ok (Term.Typed_hole env)

    | Ast.Var qual_id -> find_var t env ann qual_id

  in { Term.ann = ann; term = term }

and desugar_rec_bindings self checker env bindings =
  let open Result.Let_syntax in
  let%bind env, bindings =
    List.fold_right
      ~f:(fun { Ast.rec_lhs = str; rec_rhs = expr; rec_ann = ann } acc ->
        let%bind env, list = acc in
        let id = fresh_ident self (Some str) in
        match Env.add env str id with
        | Some env ->
           Ok (env, (ann, id, expr) :: list)
        | None -> Message.error expr.Ast.expr_ann (Message.Redefined_name str)
      ) ~init:(Ok (env, [])) bindings
  in
  let%map bindings =
    List.fold_right ~f:(fun (ann, id, expr) acc ->
        let%bind list = acc in
        let%map term = term_of_expr self checker env expr in
        { Term.rec_ann = ann; rec_lhs = id; rec_rhs = term } :: list
      ) ~init:(Ok []) bindings
  in (env, bindings)

and desugar_let_bindings self checker env bindings =
  let open Result.Let_syntax in
  let helper map { Ast.let_lhs = pat; let_rhs = expr; let_ann = _ann } =
    let%bind pat, map = pattern_of_ast_pattern self map None pat in
    let%map term = term_of_expr self checker env expr in
    (pat, term, map)
  in
  let map = Map.empty (module String) in
  let%map map, scruts, pats =
    List.fold_right ~f:(fun binding acc ->
        let%bind map, scruts, pats = acc in
        let%map pat, term, map = helper map binding in
        (map, term :: scruts, pat :: pats)
      ) ~init:(Ok (map, [], [])) bindings
  in
  let ids =
    Map.fold ~f:(fun ~key:_ ~data:id acc ->
        Set.add acc id
      ) ~init:(Set.empty (module Ident)) map
  in (map, scruts, ids, pats)

let load_import t
      { Ast.import_package = package
      ; import_path = path
      ; import_alias = alias
      ; _ } =
  match alias with
  | Some alias ->
     let prefix = { Qual_id.Prefix.package; path } in
     begin match Hashtbl.find t.packages prefix with
     | Some package ->
        Ok (Hashtbl.set t.imports ~key:alias ~data:package)
     | None -> Error (Message.Unresolved_prefix prefix)
     end
  | None -> Ok ()

let set_levels_of_tvars product =
  let helper idx =
    let rec f = function
      | Type.App(tcon, targ) ->
         f tcon;
         f targ
      | Type.Var ({ contents =
                      Type.Rigid ({ rigid_purity = Impure; _ } as tvar) } as r)
        ->
         r :=
           Type.Rigid
             { tvar with rigid_purity = Type.Impure; rigid_lam_level = idx }
      | _ -> ()
    in function
    | Type.App(Type.Prim Type.Ref, ty) -> f ty
    | _ -> ()
  in List.iteri ~f:helper product

(** Convert an [Ast.adt] into a [Type.adt] *)
let type_adt_of_ast_adt t checker adt =
  let open Result.Monad_infix in
  let tparams = List.map ~f:(fun x -> x, Ast.Pure) adt.Ast.adt_params in
  let tparams = fresh_kinds_of_typeparams checker tparams in
  let kinds = List.map ~f:(fun (k, _, _) -> k) tparams in
  let kind = Kind.curry kinds Kind.Mono in
  let constr_map = Hashtbl.create (module String) in
  List.fold_right
    adt.Ast.adt_datacons
    ~init:(Ok ([], List.length adt.Ast.adt_datacons - 1))
    ~f:(fun { Ast.datacon_name = name
            ; datacon_product = product
            ; datacon_ann = ann } acc ->
      acc >>= fun (constr_list, idx) ->
      match Hashtbl.add constr_map ~key:name ~data:idx with
      | `Duplicate -> Message.error ann (Message.Redefined_constr name)
      | `Ok ->
         (let tvar_map = Env.empty (module String) in
          Message.at ann (tvars_of_typeparams checker tvar_map tparams)
          >>= fun (tvar_map, tvar_list) ->
          List.fold_right ~f:(fun ty acc ->
              acc >>= fun products ->
              normalize t tvar_map ty >>= fun ty ->
              Message.at ann (Typecheck.kind_of_type checker ty) >>= fun kind ->
              Message.at ann (Typecheck.unify_kinds kind Kind.Mono)
              >>| fun () ->
              ty :: products
            ) ~init:(Ok []) product
          >>| fun product ->
          let out_ty =
            Type.with_params
              (Type.Nominal
                 { Qual_id.prefix = checker.package.Package.prefix
                 ; name = adt.Ast.adt_name })
              (List.map ~f:(fun var ->
                   Type.Var (ref (Type.Rigid var))
                 ) tvar_list)
          in
          set_levels_of_tvars product;
          ((name, tvar_list, product, out_ty) :: constr_list, idx - 1))
    ) >>| fun (datacons, _) ->
  let datacons = Array.of_list datacons in
  { Type.name = adt.Ast.adt_name
  ; adt_kind = kind
  ; datacon_names = constr_map
  ; datacons }

let desugar typechecker env package packages ast_file =
  let open Result.Let_syntax in
  let t = create package packages in
  let%bind () =
    List.fold_result ast_file.Ast.file_imports ~init:() ~f:(fun () import ->
        Message.at import.Ast.import_ann (load_import t import)
      ) in
  let%map env, list =
    List.fold ast_file.Ast.file_items ~init:(Ok (env, [])) ~f:(fun acc next ->
        let%bind (env, list) = acc in
        match next.Ast.item_node with
        | Ast.Let bindings ->
           let%bind map, scruts, ids, pats =
             desugar_let_bindings t typechecker env bindings in
           let%map env =
             Map.fold map ~init:(Ok env) ~f:(fun ~key:key ~data:data acc ->
                 let%bind env = acc in
                 match Env.add env key data with
                 | Some env -> Ok env
                 | None ->
                    Message.error next.Ast.item_ann (Message.Redefined_name key)
               ) in
           ( env
           , { Term.item_ann = next.Ast.item_ann
             ; item_node = Term.Top_let(scruts, ids, pats) }::list )
        | Ast.Let_rec bindings ->
           let%map env, bindings =
             desugar_rec_bindings t typechecker env bindings in
           ( env
           , { Term.item_ann = next.Ast.item_ann
             ; item_node = Term.Top_let_rec bindings }::list )
        | Ast.Type(adt, adts) ->
           let%bind () =
             List.fold ~f:(fun acc adt ->
                 let%bind () = acc in
                 let kvar = Kind.fresh_var typechecker.Typecheck.kvargen in
                 Message.at adt.Ast.adt_ann
                   (Package.add_typedef
                      package adt.Ast.adt_name (Package.Todo (Kind.Var kvar)))
               ) ~init:(Ok ()) (adt :: adts)
           in
           List.fold ~f:(fun acc adt ->
               let%bind () = acc in
               let%bind adt' = type_adt_of_ast_adt t typechecker adt in
               match Package.find_typedef package adt'.Type.name with
               | None ->
                  Message.error next.Ast.item_ann
                    (Message.Unreachable_error "Typecheck ADT")
               | Some ptr ->
                  match !ptr with
                  | Package.Compiled _ ->
                     Message.error next.Ast.item_ann
                       (Message.Redefined_name adt'.Type.name)
                  | Package.Todo kind ->
                     let%bind () =
                       Message.at adt.Ast.adt_ann
                         (Typecheck.unify_kinds kind (Type.kind_of_adt adt')) in
                     let%map () =
                       Message.at adt.Ast.adt_ann
                         (Package.add_datacons package adt')
                     in
                     ptr := Package.Compiled (Type.Manifest adt')
                  | Package.Prim _ ->
                     let%map () =
                       Message.at adt.Ast.adt_ann
                         (Package.add_datacons package adt')
                     in
                     ptr := Package.Compiled (Type.Manifest adt')
             ) ~init:(Ok ()) (adt::adts) >>| fun () -> (env, list)
      )
  in
  { Term.top_ann = ast_file.Ast.file_ann
  ; exports = ast_file.Ast.file_exports
  ; env = env
  ; items = List.rev list }
