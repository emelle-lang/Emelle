(* Copyright (C) 2018-2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base

type operand = Anf.operand

type register = Ir.Register.t

type jump =
  | Break of Ir.Label.t * operand list (** Break to a basic block *)
  | Fail (** Pattern match failure *)
  | Return of operand
  | Switch of operand * (int * Ir.Label.t) list * Ir.Label.t
      (** The jump is dynamic *)

type instr =
  | Assign of register * operand * operand
  | Box of register * int * operand list
  | Box_dummy of register * int
  | Call of register * operand * operand * operand list
  | Deref of register * operand
  | Get of register * operand * int
  | Load of register * operand
  | Package of register * Qual_id.Prefix.t
  | Prim of register * string
  | Ref of register * operand
  | Set_field of operand * int * operand
  | Set_tag of operand * int
  | Tag of register * operand

type basic_block = {
    mutable preds : (Ir.Label.t, Ir.Label.comparator_witness) Set.t;
    params : register list;
    instrs : instr Queue.t;
    jump : jump;
  }

type proc = {
    free_vars : Anf.register list;
    params : Anf.register list;
    entry : Ir.Label.t;
    blocks : (Ir.Label.t, basic_block, Ir.Label.comparator_witness) Map.t;
    before_return : Ir.Label.t;
  }

type file = {
    procs : (int, proc, Int.comparator_witness) Map.t;
    main : proc
  }

let successors = function
  | Break(label, _) -> [label]
  | Return _ -> []
  | Fail -> []
  | Switch(_, cases, else_case) ->
     else_case::(List.map ~f:(fun (_, label) -> label) cases)
