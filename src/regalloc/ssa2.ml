(* Copyright (C) 2019-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

type instr = {
    dest : Ir.Register.t option;
    opcode : Ssa.instr;
    ending_regs : (Ir.Register.t, Ir.Register.comparator_witness) Set.t;
  }

type basic_block = {
    params : Ir.Register.t list;
    preds : (Ir.Label.t, Ir.Label.comparator_witness) Set.t;
    instrs : instr list;
    jump : Ssa.jump;
    ending_at_jump : (Ir.Register.t, Ir.Register.comparator_witness) Set.t;
  }

type proc = {
    free_vars : Ir.Register.t list;
    params : Ir.Register.t list;
    entry : Ir.Label.t;
    blocks : (Ir.Label.t, basic_block, Ir.Label.comparator_witness) Map.t;
    before_returns : Ir.Label.t list;
    return : Ir.Register.t;
  }

type file = {
    procs : (int, proc, Int.comparator_witness) Map.t;
    main : proc
  }
