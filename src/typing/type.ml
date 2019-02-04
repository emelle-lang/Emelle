open Base

type purity =
  | Pure
  | Impure
[@@deriving sexp]

type prim =
  | Arrow
  | Char
  | Float
  | Int
  | Ref
  | String
  | Unit
[@@deriving compare, sexp]

(** Type [quant] describes whether a type variable is existentially quantified
    within a scope or universally quantified. *)
type quant =
  | Exists of int ref
  | Univ
[@@deriving sexp]

(** Each type is annotated with the greatest level of its children. *)
type t =
  | App of t * t
  | Nominal of Path.t
  | Prim of prim
  | Var of var
[@@deriving sexp]
and var =
  { id : int
  ; mutable quant : quant
  ; mutable ty : t option
  ; mutable purity : purity
  ; mutable lam_level : int
  ; kind : Kind.t }
[@@deriving sexp]

(** Type [vargen] is the generator of fresh type variables. *)
type vargen = int ref

module Var = struct
  type t = var
  [@@deriving sexp]

  type gen = vargen

  let compare l r = compare l.id r.id
  let hash x = x.id
end

type adt = {
    name : string;
    adt_kind : Kind.t;
    constr_names : (string, int) Hashtbl.t;
    constrs : (string * t list * t) array
  }

type decl =
  | Abstract of Kind.t
  | Manifest of adt

let equal_prim x y = (compare_prim x y) = 0

(** [create_vargen ()] creates a fresh vargen state. *)
let create_vargen () = { contents = 0 }

let fresh_var vargen purity quant lam_level kind =
  vargen := !vargen + 1;
  { id = !vargen - 1
  ; ty = None
  ; quant
  ; purity
  ; lam_level
  ; kind = kind }

let arrow l r =
  let arrow = Prim Arrow in
  let ty = App(arrow, l) in
  App(ty, r)

(** [with_params ty \[a; b; ...; z\]] is (... ((ty a) b) ...z) *)
let with_params ty =
  List.fold ~f:(fun acc param -> App(acc, param)) ~init:ty

(** [curry \[a; b; ...; z\] ty] is a -> b -> ... z -> ty.
    [curry \[\] ty] is ty. *)
let rec curry input_tys output_ty =
  match input_tys with
  | [] -> output_ty
  | (ty::tys) ->
     let out_ty = curry tys output_ty in
     arrow ty out_ty

(** Given an ADT and one of its constructors, return the constructor's type *)
let type_of_constr adt constr =
  let _, product, output_ty = adt.constrs.(constr) in
  curry product output_ty

let kind_of_adt adt = adt.adt_kind

let kind_of_prim = function
  | Arrow -> Kind.Poly(Kind.Mono, Kind.Poly(Kind.Mono, Kind.Mono))
  | Char -> Kind.Mono
  | Float -> Kind.Mono
  | Int -> Kind.Mono
  | Ref -> Kind.Poly(Kind.Mono, Kind.Mono)
  | String -> Kind.Mono
  | Unit -> Kind.Mono

(** [occurs tvar ty] performs the occurs check, returning true if [tyvar] occurs
    in [ty]. It ignores universally quantified type variables and adjusts the
    levels of unassigned typevars when necessary. *)
let rec occurs tvar ty =
  match ty with
  | App(tcon, targ) ->
     occurs tvar tcon || occurs tvar targ
  | Nominal _ -> false
  | Prim _ -> false
  | Var { id; _ } when id = tvar.id -> true
  | Var { ty = Some ty; _ } -> occurs tvar ty
  | Var tvar2 ->
     begin match tvar.quant, tvar2.quant with
     | Exists let_level, Exists let_level2 ->
        if !let_level2 > !let_level then (
          let_level2 := !let_level
        );
        begin match tvar.purity with
        | Impure ->
           if tvar2.lam_level > tvar.lam_level then
             tvar2.lam_level <- tvar.lam_level
        | Pure -> ()
        end
     | _ -> ()
     end;
     begin match tvar2.purity with
     | Pure ->
        tvar2.purity <- tvar.purity
     | _ -> ()
     end;
     false

let rec decr_lam_levels level = function
  | App(tcon, targ) ->
     decr_lam_levels level tcon;
     decr_lam_levels level targ
  | Nominal _ -> ()
  | Prim _ -> ()
  | Var { ty = Some ty; _ } -> decr_lam_levels level ty
  | Var tvar ->
     match tvar.quant with
     | Exists _ ->
        if tvar.lam_level > level && tvar.lam_level > 0 then
          tvar.lam_level <- tvar.lam_level - 1
     | Univ -> ()
