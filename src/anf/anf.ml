(* Copyright (C) 2018-2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base

type register = Ir.Register.t

type operand = Ir.Operand.t

(** A jump is a branch instruction to a join point with the given arguments *)
and jump = operand list * int

type decision_tree =
  | Deref of operand * register * decision_tree
  | Fail
  | Leaf of jump
    (** A leaf holds a mapping from idents to pattern match occurrences. *)
  | Switch of
      register * operand
      * (int, register list * decision_tree, Int.comparator_witness) Map.t
      * decision_tree
    (** A switch holds the scrutinee occurrence and a map from constructors to
        decision trees, and a default decision tree. *)

type 'a opcode =
  | Assign of operand * operand
  | Box of int * operand list
  | Call of operand * operand * operand list
    (** proc, first arg, rest args *)
  | Case of decision_tree * 'a join_point list
    (** decision tree, jump table *)
  | Fun of 'a proc
  | Get of operand * int
  | Load of operand
  | Package of Qual_id.Prefix.t
  | Prim of string
  | Ref of operand

(** A join point is like a basic block with parameters *)
and 'a join_point = register list * 'a instr

and 'a instr' =
  | Break of operand
  | Let of register * 'a opcode * 'a instr
  | Let_rec of (register * 'a opcode) list * 'a instr

and 'a instr = {
    instr : 'a instr';
    ann : 'a;
  }

and 'a proc = {
    env : (register * operand) list; (** The captured variables *)
    params : register list;
    body : 'a instr;
    reg_gen : Ir.Register.gen;
  }

type 'a file = {
    top_instr : 'a instr;
    reg_gen : Ir.Register.gen;
  }
