(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

type address = int

type operand =
  | Lit of Literal.t
  | Stack of address

type instr =
  | Assign of address * operand * operand
  | Box of address * int * operand list
  | Box_dummy of address * int
  | Call of address * operand * operand * operand list
  | Deref of address * operand
  | Get of address * operand * int
  | Move of address * operand
  | Package of address * Qual_id.Prefix.t
  | Prim of address * string
  | Ref of address * operand
  | Set_field of operand * int * operand
  | Set_tag of operand * int
  | Tag of address * operand
  | Tail_call of operand * operand * operand list
  | Break of Ir.Label.t
  | Fail
  | Return
  | Switch of operand * (int * Ir.Label.t) list * Ir.Label.t

type block = {
    block_params : address list;
    instrs : instr Queue.t;
  }

type proc = {
    free_vars : address list;
    params : address list;
    entry : Ir.Label.t;
    blocks : (Ir.Label.t, block, Ir.Label.comparator_witness) Map.t;
    frame_size : int;
    return : int;
  }

type file = {
    procs : (int, proc, Int.comparator_witness) Map.t;
    main : proc
  }
