(* Copyright (C) 2018-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base

type t =
  | Mono
  | Poly of t * t
  | Var of var
[@@deriving sexp]
and var =
  { id : int
  ; mutable kind : t option }

type vargen = int ref

let create_vargen () = ref 0

let fresh_var vargen =
  let id = !vargen in
  vargen := id + 1;
  { id = id; kind = None }

let rec curry input_ks output_k =
  match input_ks with
  | [] -> output_k
  | (k::ks) -> Poly(k, curry ks output_k)

let rec occurs kvar = function
  | Mono -> false
  | Poly(k1, k2) -> occurs kvar k1 || occurs kvar k2
  | Var { id; _ } when id = kvar.id -> true
  | Var { kind = Some kind; _ } -> occurs kvar kind
  | Var { kind = None; _ } -> false
