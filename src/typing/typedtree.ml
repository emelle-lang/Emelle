(* Copyright (C) 2018-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base

type bindings = (Ident.t, Ident.comparator_witness) Set.t

type ('ann, 'a) expr =
  | App of 'a * 'a
  | Assign of 'a * 'a
  | Case of 'a list * 'ann Pattern.matrix * (bindings * 'a) list
  | Constr of int * int
  | Extern_var of Qual_id.Prefix.t * int
  | Field_access of 'a * int
  | Lam of Ident.t * 'a
  | Let_rec of (Ident.t * 'a) list * 'a
  | Let of Ident.t * 'a * 'a
  | Lit of Literal.t
  | Local_var of Ident.t
  | Prim of string
  | Record of 'a list
  | Ref
  | Seq of 'a * 'a
  | Typed_hole of Term.env * (Ident.t, Type.polytype) Hashtbl.t * Type.t

type 'a t = {
    ann : 'a;
    ty : Type.t;
    expr : ('a, 'a t) expr
  }

type 'a item' =
  | Top_let of
      'a t list * (Ident.t, Ident.comparator_witness) Set.t * 'a Pattern.matrix
  | Top_let_rec of (Ident.t * 'a t) list

type 'a item = {
    item_ann : 'a;
    item_node : 'a item'
  }

type 'a file = {
    top_ann : 'a;
    items : 'a item list;
    exports : string list;
    env : (string, Ident.t, String.comparator_witness) Env.t;
    typing_ctx : (Ident.t, Type.polytype) Hashtbl.t
  }
