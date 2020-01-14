(* Copyright (C) 2018-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

type env = (string, Ident.t, String.comparator_witness) Env.t

type ('ann, 'fix) term =
  | App of 'fix * 'fix
  | Assign of 'fix * 'fix
  | Case of 'fix list * ('ann, 'fix) branch list
  | Constr of Type.adt * int
  | Extern_var of Qual_id.Prefix.t * int * Type.polytype
  | Field_access of Type.record * 'fix * int
  | Lam of Ident.t * 'fix
  | Let of Ident.t * 'fix * 'fix
  | Let_rec of ('ann, 'fix) bind_group * 'fix
  | Lit of Literal.t
  | Prim of string * Type.polytype
  | Record of ('ann, 'fix) record
  | Ref
  | Seq of 'fix * 'fix
  | Typed_hole of env
  | Var of Ident.t

and ('a, 'fix) rec_binding = {
    rec_ann : 'a;
    rec_lhs : Ident.t;
    rec_rhs : 'fix;
  }

and ('a, 'fix) record = {
    record : Type.record;
    fields : (string * Type.polytype * 'fix) list;
  }

and ('a, 'fix) bind_group = ('a, 'fix) rec_binding list

and id_set = (Ident.t, Ident.comparator_witness) Set.t

and ('ann, 'term) branch = 'ann Pattern.t list * id_set * 'term

type 'ann t = {
    term : ('ann, 'ann t) term;
    ann : 'ann;
  }

type 'a item' =
  | Top_let of
      'a t list * (Ident.t, Ident.comparator_witness) Set.t * 'a Pattern.t list
  | Top_let_rec of ('a, 'a t) bind_group

type 'a item = {
    item_ann : 'a;
    item_node : 'a item';
  }

type 'a file = {
    top_ann : 'a;
    exports : string list;
    env : env;
    items : 'a item list;
  }
