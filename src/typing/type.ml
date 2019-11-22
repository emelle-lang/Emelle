(* Copyright (C) 2018-2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

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

(** A type variable is either:
    - Infinite
    - Rigid
    - Solved
    - Wobbly *)
type var =
  | Rigid of rigid_var
  | Solved of t
  | Wobbly of wobbly_var
[@@deriving sexp]

and t =
  | App of t * t
  | Nominal of Qual_id.t
  | Prim of prim
  | Var of var ref
[@@deriving sexp]

and rigid_var =
  { rigid_id : int
  ; rigid_purity : purity
  ; rigid_lam_level : int
  ; rigid_kind : Kind.t }
[@@deriving sexp]

and wobbly_var = {
    wobbly_id : int;
    mutable wobbly_purity : purity;
    mutable wobbly_lam_level : int;
    wobbly_kind : Kind.t;
    mutable wobbly_let_level : int;
  }

type polytype = Forall of rigid_var list * t

(** Type [vargen] is the generator of fresh type variables. *)
type vargen = int ref

type adt = {
    name : string;
    adt_kind : Kind.t;
    datacon_names : (string, int) Hashtbl.t;
    datacons : (string * rigid_var list * t list * t) array
  }

type record = {
    record_name : string;
    record_kind : Kind.t;
    record_tparams : rigid_var list;
    field_names : (string, int) Hashtbl.t;
    fields : (string * polytype) array;
  }

type decl =
  | Abstract of Kind.t
  | Adt of adt
  | Record of record

let equal_prim x y = (compare_prim x y) = 0

(** [create_vargen ()] creates a fresh vargen state. *)
let create_vargen () = { contents = 0 }

let fresh_wobbly vargen purity ~let_level ~lam_level kind =
  let id = !vargen in
  vargen := id + 1;
  { wobbly_id = id
  ; wobbly_purity = purity
  ; wobbly_lam_level = lam_level
  ; wobbly_kind = kind
  ; wobbly_let_level = let_level }

let fresh_rigid vargen purity lam_level kind =
  let id = !vargen in
  vargen := id + 1;
  { rigid_id = id
  ; rigid_purity = purity
  ; rigid_lam_level = lam_level
  ; rigid_kind = kind }

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
  | ty :: tys ->
     let out_ty = curry tys output_ty in
     arrow ty out_ty

(** Given an ADT and one of its constructors, return the constructor's type *)
let type_of_constr adt constr =
  let _, tvars, product, output_ty = adt.datacons.(constr) in
  Forall(tvars, curry product output_ty)

let kind_of_prim = function
  | Arrow -> Kind.Poly(Kind.Mono, Kind.Poly(Kind.Mono, Kind.Mono))
  | Char -> Kind.Mono
  | Float -> Kind.Mono
  | Int -> Kind.Mono
  | Ref -> Kind.Poly(Kind.Mono, Kind.Mono)
  | String -> Kind.Mono
  | Unit -> Kind.Mono

let rec decr_lam_levels level = function
  | App(tcon, targ) ->
     decr_lam_levels level tcon;
     decr_lam_levels level targ
  | Nominal _ -> ()
  | Prim _ -> ()
  | Var { contents = Solved ty } -> decr_lam_levels level ty
  | Var { contents = Wobbly wobbly } ->
     if wobbly.wobbly_lam_level > level && wobbly.wobbly_lam_level > 0 then
       wobbly.wobbly_lam_level <- wobbly.wobbly_lam_level - 1
  | Var { contents = Rigid _ } -> ()
