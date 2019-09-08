(* Copyright (C) 2018-2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

type t = {
    package : Package.t;
    packages : (Qual_id.Prefix.t, Package.t) Hashtbl.t;
    env : (Ident.t, Type.t) Hashtbl.t;
    let_level : int;
    lam_level : int;
    tvargen : Type.vargen;
    kvargen : Kind.vargen
  }

(** [create symtable] creates a fresh typechecker state. *)
let create package packages =
  { package
  ; packages
  ; env = Hashtbl.create (module Ident)
  ; let_level = 0
  ; lam_level = 0
  ; tvargen = Type.create_vargen ()
  ; kvargen = Kind.create_vargen () }

let fresh_tvar ?(purity = Type.Pure) (checker : t) =
  Type.Var
    (ref (Type.Wobbly
            (Type.fresh_wobbly
               checker.tvargen
               purity
               ~let_level:checker.let_level
               ~lam_level:checker.lam_level
               Kind.Mono))
    )

let find f st { Qual_id.prefix; name } =
  match Hashtbl.find st.packages prefix with
  | None -> None
  | Some package -> f package name

(** [unify_kinds kind1 kind2] unifies the two kinds. *)
let rec unify_kinds l r =
  let open Result.Monad_infix in
  match l, r with
  | Kind.Mono, Kind.Mono -> Ok ()
  | Kind.Poly(a, b), Kind.Poly(c, d) ->
     unify_kinds a c >>= fun () ->
     unify_kinds b d
  | Kind.Var { id = l; _ }, Kind.Var { id = r; _ } when l = r -> Ok ()
  | Kind.Var { kind = Some k1; _ }, k2 | k2, Kind.Var { kind = Some k1; _ } ->
     unify_kinds k1 k2
  | Kind.Var kvar, kind | kind, Kind.Var kvar ->
     if Kind.occurs kvar kind then
       Error (Message.Kind_unification_fail(l, r))
     else (
       kvar.kind <- Some kind;
       Ok ()
     )
  | Kind.Mono, Kind.Poly _ | Kind.Poly _, Kind.Mono ->
     Error (Message.Kind_unification_fail(l, r))

(** [kind_of_type typechecker ty] infers the kind of [ty], returning a result
    with any errors. *)
let rec kind_of_type checker ty =
  let open Result.Monad_infix in
  match ty with
  | Type.App(tcon, targ) ->
     kind_of_type checker targ >>= fun argkind ->
     kind_of_type checker tcon >>= fun conkind ->
     let kvar = Kind.Var (Kind.fresh_var checker.kvargen) in
     unify_kinds conkind (Kind.Poly(argkind, kvar)) >>| fun () ->
     kvar
  | Type.Nominal path ->
     begin match find Package.kind_of_ident checker path with
     | Some kind -> Ok kind
     | None -> Error (Message.Unresolved_type path)
     end
  | Type.Prim prim -> Ok (Type.kind_of_prim prim)
  | Type.Var { contents = Solved ty } -> kind_of_type checker ty
  | Type.Var { contents = Wobbly wobbly } -> Ok wobbly.Type.wobbly_kind
  | Type.Var { contents = Rigid var } -> Ok var.Type.rigid_kind

(** [unify_types typechecker type0 type1] unifies [type0] and [type1], returning
    a result with any unification errors. *)
let rec unify_types checker lhs rhs =
  let open Result.Let_syntax in
  if phys_equal lhs rhs then
    Ok ()
  else
    match lhs, rhs with
    | Type.App(lcon, larg), Type.App(rcon, rarg) ->
       begin
         match unify_types checker lcon rcon, unify_types checker larg rarg with
         | Ok (), Ok () -> Ok ()
         | Error e, Ok () | Ok (), Error e -> Error e
         | Error e1, Error e2 -> Error (Message.And_error(e1, e2))
       end
    | Type.Nominal lstr, Type.Nominal rstr
         when (Qual_id.compare lstr rstr) = 0 ->
       Ok ()
    | Type.Prim lprim, Type.Prim rprim
         when Type.equal_prim lprim rprim ->
       Ok ()
    | Type.Var { contents = Wobbly wobbly1 }
    , Type.Var { contents = Wobbly wobbly2 }
         when wobbly1.wobbly_id = wobbly2.wobbly_id ->
       Ok ()
    | Type.Var { contents = Solved ty0 }, ty1
    | ty0, Type.Var { contents = Solved ty1 } ->
       unify_types checker ty0 ty1
    | Type.Var ({ contents = Wobbly wobbly } as r), ty
    | ty, Type.Var ({ contents = Wobbly wobbly } as r) ->
       if Type.occurs wobbly ty then
         Error (Message.Occurs(wobbly, ty))
       else
         let%bind kind = kind_of_type checker ty in
         let%map () = unify_kinds kind wobbly.wobbly_kind in
         r := Type.Solved ty
    | _ -> Error (Message.Type_unification_fail(lhs, rhs))

let in_new_let_level f self =
  f { self with let_level = self.let_level + 1 }

let in_new_lam_level f self =
  f { self with lam_level = self.lam_level + 1 }

let rigid_of_wobbly checker wobbly =
  { Type.rigid_id = wobbly.Type.wobbly_id
  ; rigid_purity = wobbly.wobbly_purity
  ; rigid_lam_level = wobbly.wobbly_lam_level - checker.lam_level
  ; rigid_kind = wobbly.wobbly_kind }

let wobbly_of_rigid checker rigid =
  let id = !(checker.tvargen) in
  checker.tvargen := id + 1;
  { Type.wobbly_id = id
  ; wobbly_purity = rigid.Type.rigid_purity
  ; wobbly_lam_level = rigid.rigid_lam_level + checker.lam_level
  ; wobbly_kind = rigid.rigid_kind
  ; wobbly_let_level = checker.let_level }

(** [gen checker ty] generalizes a type by replacing existential type variables
    of level [checker.level] or higher with a universally quantified variable.
    Universally quantified variables really shouldn't appear in [ty], but the
    function just ignores them. *)
let gen checker =
  let rec helper ty =
    match ty with
    | Type.App(tcon, targ) ->
       helper tcon;
       helper targ
    | Type.Var { contents = Solved ty } -> helper ty
    | Type.Var ({ contents = Wobbly wobbly } as r) ->
       let test =
         match wobbly.wobbly_purity with
         | Pure -> wobbly.wobbly_let_level >= checker.let_level
         | Impure ->
            (wobbly.wobbly_let_level >= checker.let_level) &&
              (wobbly.wobbly_lam_level > checker.lam_level)
       in
       if test then (
         r := Type.Rigid (rigid_of_wobbly checker wobbly)
       )
    | _ -> ()
  in helper

(** [inst checker map polyty] instantiates [polyty] by replacing universally
    quantified type variables with type variables of level [checker.let_level]
 *)
let inst checker map =
  let rec helper ty =
    match ty with
    | Type.App(tcon, targ) -> Type.App(helper tcon, helper targ)
    | Type.Var { contents = Solved ty } -> helper ty
    | Type.Var { contents = Rigid rigid } ->
       Hashtbl.find_or_add map rigid.rigid_id ~default:(fun () ->
           Type.Var (ref (Type.Wobbly (wobbly_of_rigid checker rigid)))
         )
    | _ -> ty
  in helper

let make_impure checker ty =
  let tvar =
    Type.fresh_wobbly
      checker.tvargen
      Type.Impure
      ~let_level:checker.let_level
      ~lam_level:checker.lam_level
      Kind.Mono in
  let _ = Type.occurs tvar ty in
  ()

(** [infer_pattern checker map ty pat] associates [ty] with [pat]'s register
    if it has any while unifying any type constraints that arise from [pat]. *)
let rec infer_pattern checker map ty pat =
  let open Result.Monad_infix in
  let type_binding pat =
    match pat.Pattern.id with
    | None -> Ok map
    | Some id ->
       (* The binding could already exist because of a prior OR pattern
          alternative *)
       match Map.find map id with
       | Some ty2 ->
          Message.at pat.Pattern.ann (unify_types checker ty ty2)
          >>| fun () -> map
       | None ->
          match Map.add map ~key:id ~data:ty with
          | `Ok map -> Ok map
          | `Duplicate ->
             Message.error pat.Pattern.ann
               (Message.Unreachable_error "infer_pattern dup")
  in
  match pat.Pattern.node with
  | Pattern.Con(adt, idx, pats) ->
     let tvar_map = Hashtbl.create (module Int) in
     let (_, products, adt_ty) = adt.Type.datacons.(idx) in
     let nom_ty = inst checker tvar_map adt_ty in
     let products = List.map ~f:(inst checker tvar_map) products in
     Message.at pat.Pattern.ann (unify_types checker ty nom_ty) >>= fun () ->
     type_binding pat >>= fun map ->
     let rec f map pats tys =
       match pats, tys with
       | [], [] -> Ok map
       | [], _  -> Message.error pat.Pattern.ann (Message.Not_enough_fields)
       | _, [] -> Message.error pat.Pattern.ann (Message.Too_many_fields)
       | pat :: pats, ty :: tys ->
          infer_pattern checker map ty pat >>= fun map ->
          f map pats tys
     in f map pats products
  | Pattern.Deref subpat ->
     let tvar = fresh_tvar checker in
     make_impure checker tvar;
     let ref_ty = Type.App(Type.Prim Type.Ref, tvar) in
     Message.at pat.Pattern.ann (unify_types checker ty ref_ty) >>= fun () ->
     type_binding pat >>= fun map ->
     infer_pattern checker map tvar subpat
  | Pattern.Unit ->
     Message.at pat.Pattern.ann (unify_types checker ty (Type.Prim Type.Unit))
     >>| fun () -> map
  | Pattern.Wild -> type_binding pat
  | Pattern.Or(p1, p2) ->
     infer_pattern checker map ty p1 >>= fun map1 ->
     infer_pattern checker map ty p2 >>= fun map2 ->
     Map.fold2 map1 map2 ~init:(Ok ()) ~f:(fun ~key:_ ~data acc ->
         acc >>= fun () ->
         match data with
         | `Both(t1, t2) ->
            Message.at pat.Pattern.ann (unify_types checker t1 t2)
         | _ -> Message.error pat.Pattern.ann (Message.Unreachable_error "f")
       ) >>| fun () ->
     Map.merge_skewed map1 map ~combine:(fun ~key:_ _ v -> v)

(** [infer_expr typechecker term] infers the type of [term], returning a
    result. *)
let rec infer_term checker Term.{ term; ann } =
  let open Result.Monad_infix in
  match term with
  | Term.App(f, x) ->
     begin match infer_term checker f, infer_term checker x with
     | (Ok f, Ok x) ->
        begin match f.Typedtree.ty with
        | Type.App(Type.App(Type.Prim Type.Arrow, _domain), codomain) ->
           Type.decr_lam_levels checker.lam_level codomain;
        | _ -> ()
        end;
        let var = fresh_tvar checker in
        Message.at ann
          (unify_types checker f.Typedtree.ty (Type.arrow x.Typedtree.ty var))
        >>| fun () ->
        { Typedtree.ann; ty = var; expr = Typedtree.App(f, x) }
     | (Error f_err, Error x_err) -> Error (Message.And(f_err, x_err))
     | (err, Ok _) | (Ok _, err) -> err
     end

  | Term.Assign(lval, rval) ->
     infer_term checker lval >>= fun lval ->
     infer_term checker rval >>= fun rval ->
     Message.at ann
       (unify_types checker
          lval.Typedtree.ty
          (Type.App(Type.Prim Type.Ref, rval.Typedtree.ty))) >>| fun () ->
     make_impure checker rval.Typedtree.ty;
     { Typedtree.ann
     ; ty = Type.Prim Type.Unit
     ; expr = Typedtree.Assign(lval, rval) }

  | Term.Case(scrutinees, cases) ->
     let out_ty = fresh_tvar checker in
     List.fold_right ~f:(fun scrutinee acc ->
         acc >>= fun list ->
         in_new_let_level (fun checker ->
             infer_term checker scrutinee
           ) checker >>| fun expr ->
         expr::list
       ) ~init:(Ok []) scrutinees >>= fun scruts ->
     List.fold_right ~f:(fun (pats, ids, consequent) acc ->
         acc >>= fun (idx, matrix, branches) ->
         infer_branch checker scruts pats >>= fun () ->
         infer_term checker consequent >>= fun consequent ->
         Message.at ann (unify_types checker consequent.Typedtree.ty out_ty)
         >>| fun () ->
         ( idx - 1
         , { Pattern.patterns = pats
           ; bindings = Map.empty (module Ident)
           ; action = idx }::matrix
         , (ids, consequent)::branches )
       ) ~init:(Ok (List.length cases - 1, [], [])) cases
     >>| fun (_, matrix, branches) ->
     { Typedtree.ann
     ; ty = out_ty
     ; expr = Typedtree.Case(scruts, matrix, branches) }

  | Term.Constr(adt, idx) ->
     let _, product, out_ty = adt.Type.datacons.(idx) in
     let ty = Type.curry product out_ty in
     Ok { Typedtree.ann
        ; ty = inst checker (Hashtbl.create (module Int)) ty
        ; expr = Typedtree.Constr(idx, List.length product) }

  | Term.Extern_var(prefix, offset, ty) ->
     Ok { Typedtree.ann
        ; ty = inst checker (Hashtbl.create (module Int)) ty
        ; expr = Typedtree.Extern_var(prefix, offset) }

  | Term.Lam(id, body) ->
     in_new_lam_level (fun checker ->
         let var = fresh_tvar checker in
         Hashtbl.add_exn checker.env ~key:id ~data:var;
         infer_term checker body >>| fun body ->
         { Typedtree.ann
         ; ty = Type.arrow var body.Typedtree.ty
         ; expr = Typedtree.Lam(id, body) }
       ) checker

  | Term.Let(lhs, rhs, body) ->
     in_new_let_level (fun checker ->
         infer_term checker rhs
       ) checker >>= fun rhs ->
     gen checker rhs.Typedtree.ty;
     Hashtbl.add_exn checker.env ~key:lhs ~data:rhs.Typedtree.ty;
     infer_term checker body >>| fun body ->
     { Typedtree.ann
     ; ty = body.Typedtree.ty
     ; expr = Typedtree.Let(lhs, rhs, body) }

  | Term.Let_rec(bindings, body) ->
     infer_rec_bindings checker bindings >>= fun bindings ->
     (* In the RHS of let-rec bindings, LHS names aren't quantified. Here,
        quantify them for the let-rec body. *)
     List.iter ~f:(fun (lhs, _) ->
         match Hashtbl.find checker.env lhs with
         | Some ty -> gen checker ty
         | None -> ()
       ) bindings;
     infer_term checker body >>| fun body ->
     { Typedtree.ann
     ; ty = body.Typedtree.ty; expr = Typedtree.Let_rec(bindings, body) }

  | Term.Lit lit ->
     Ok { Typedtree.ann
        ; ty =
            begin match lit with
            | Literal.Char _ -> Type.Prim Type.Char
            | Literal.Float _ -> Type.Prim Type.Float
            | Literal.Int _ -> Type.Prim Type.Int
            | Literal.String _ -> Type.Prim Type.String
            | Literal.Unit -> Type.Prim Type.Unit
            end
        ; expr = Typedtree.Lit lit }

  | Term.Prim(op, ty) ->
     Ok { Typedtree.ann
        ; ty = inst checker (Hashtbl.create (module Int)) ty
        ; expr = Typedtree.Prim op }

  | Term.Ref ->
     in_new_lam_level (fun checker ->
         let var = fresh_tvar checker in
         make_impure checker var;
         Ok { Typedtree.ann
            ; ty = Type.arrow var (Type.App(Type.Prim Type.Ref, var))
            ; expr = Typedtree.Ref }
       ) checker

  | Term.Seq(s, t) ->
     infer_term checker s >>= fun s ->
     Message.at ann (unify_types checker s.Typedtree.ty (Type.Prim Type.Unit))
     >>= fun () ->
     infer_term checker t >>| fun t ->
     { Typedtree.ann; ty = t.Typedtree.ty; expr = Typedtree.Seq(s, t) }

  | Term.Typed_hole env ->
     let ty = fresh_tvar checker in
     Ok { Typedtree.ann; ty; expr = Typedtree.Typed_hole(env, checker.env, ty) }

  | Term.Var id ->
     match Hashtbl.find checker.env id with
     | Some ty ->
        Ok { Typedtree.ann
           ; ty = inst checker (Hashtbl.create (module Int)) ty
           ; expr = Typedtree.Local_var id }
     | None -> Message.error ann (Message.Unreachable_error "Tc expr var")

and infer_branch checker scruts pats =
  let open Result.Monad_infix in
  let rec f map scruts pats =
    match scruts, pats with
    | [], [] -> Ok map
    | [], _ | _, [] -> Message.unreachable "infer_branch"
    | scrut::scruts, pat::pats ->
       infer_pattern checker map scrut.Typedtree.ty pat
       >>= fun map ->
       f map scruts pats
  in
  let map = Map.empty (module Ident) in
  in_new_let_level (fun checker ->
      f map scruts pats >>| fun map ->
      Map.iteri ~f:(fun ~key ~data ->
          gen checker data;
          let _ = Hashtbl.add checker.env ~key ~data in
          ()
        ) map
    ) checker

and infer_rec_bindings checker bindings =
  let open Result.Monad_infix in
  in_new_let_level (fun checker ->
      (* Associate each new binding with a fresh type variable *)
      let f { Term.rec_lhs = lhs; _ } =
        Hashtbl.add_exn checker.env ~key:lhs ~data:(fresh_tvar checker)
      in
      List.iter ~f:f bindings;
      (* Type infer the RHS of each new binding and unify the result with
         the type variable *)
      let f { Term.rec_ann = ann; rec_lhs = lhs; rec_rhs = rhs } acc =
        let tvar = Hashtbl.find_exn checker.env lhs in
        infer_term checker rhs >>= fun rhs ->
        match
          acc, Message.at ann (unify_types checker tvar rhs.Typedtree.ty)
        with
        | Ok acc, Ok () -> Ok ((lhs, rhs)::acc)
        | Ok _, Error e | Error e, Ok () -> Error e
        | Error e1, Error e2 -> Error (Message.And(e1, e2))
      in List.fold_right ~f:f ~init:(Ok []) bindings
    ) checker

let typecheck typechecker term_file =
  let open Result.Monad_infix in
  List.fold ~f:(fun acc next ->
      acc >>= fun list ->
      match next.Term.item_node with
      | Term.Top_let(scruts, ids, pats) ->
         List.fold_right ~f:(fun scrut acc ->
             acc >>= fun list ->
             in_new_let_level (fun typechecker ->
                 infer_term typechecker scrut
               ) typechecker >>| fun expr ->
             expr::list
           ) ~init:(Ok []) scruts >>= fun scruts ->
         infer_branch typechecker scruts pats >>| fun () ->
         let matrix =
           [ { Pattern.patterns = pats
             ; bindings = Map.empty (module Ident)
             ; action = 0 } ] in
         { Typedtree.item_ann = next.Term.item_ann
         ; item_node = Typedtree.Top_let(scruts, ids, matrix) }::list
      | Term.Top_let_rec bindings ->
         infer_rec_bindings typechecker bindings >>| fun bindings ->
         { Typedtree.item_ann = next.Term.item_ann
         ; item_node = Typedtree.Top_let_rec bindings }::list
    ) ~init:(Ok []) term_file.Term.items
  >>| fun items ->
  { Typedtree.top_ann = term_file.Term.top_ann
  ; items = List.rev items
  ; exports = term_file.Term.exports
  ; env = term_file.Term.env
  ; typing_ctx = typechecker.env }
