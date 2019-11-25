(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

let operands_of_opcode = function
  | Ssa.Assign(dest, lval, rval) -> Some dest, [lval; rval]
  | Ssa.Box(dest, _, list) -> Some dest, list
  | Ssa.Box_dummy (dest, _) -> Some dest, []
  | Ssa.Call(dest, f, arg, args) -> Some dest, f :: arg :: args
  | Ssa.Deref(dest, op) -> Some dest, [op]
  | Ssa.Get(dest, op, _) -> Some dest, [op]
  | Ssa.Load(dest, op) -> Some dest, [op]
  | Ssa.Package(dest, _) -> Some dest, []
  | Ssa.Prim(dest, _) -> Some dest, []
  | Ssa.Ref(dest, op) -> Some dest, [op]
  | Ssa.Set_field(dest, _, op) -> None, [dest; op]
  | Ssa.Set_tag(dest, _) -> None, [dest]
  | Ssa.Tag(dest, op) -> Some dest, [op]

let operands_of_jump = function
  | Ssa.Break(_, args) -> args
  | Ssa.Fail -> []
  | Ssa.Return -> []
  | Ssa.Switch(scrut, _, _) -> [scrut]
  | Ssa.Tail_call(f, arg, args) -> f :: arg :: args

let regs_of_opcode opcode =
  let dest_opt, operands = operands_of_opcode opcode in
  ( dest_opt
  , List.fold operands ~init:[] ~f:(fun acc ->
        function
        | Ir.Operand.Register reg -> reg :: acc
        | _ -> acc
  ) )

let regs_of_jump jump =
  let open List.Monad_infix in
  operands_of_jump jump >>= function
  | Ir.Operand.Register reg -> [reg]
  | _ -> []

(** [handle_regs live_regs operand_regs]

    Given a set of currently live *)
let handle_regs live_regs operand_regs =
  let operand_regs = Set.of_list (module Ir.Register) operand_regs in
  let new_regs = Set.diff operand_regs live_regs in
  (Set.union live_regs new_regs, new_regs)

let handle_instr live_regs instr =
  let dest_opt, operand_regs = regs_of_opcode instr in
  let live_regs, ending_regs = handle_regs live_regs operand_regs in
  let live_regs =
    match dest_opt with
    | Some dest -> Set.remove live_regs dest
    | None -> live_regs in
  ( { Ssa2.dest = dest_opt
    ; opcode = instr
    ; ending_regs }
  , live_regs )

(** [handle_instrs live_regs instrs] iterates over [instrs] backwards and calls
    [handle_instr live_regs instr] for each instruction. *)
let handle_instrs live_regs instrs =
  let rec go live_regs list = function
    | 0 -> list, live_regs
    | i ->
       let instr, live_regs = handle_instr live_regs (Queue.get instrs (i - 1))
       in go live_regs (instr::list) (i - 1)
  in go live_regs [] (Queue.length instrs)

let find_block proc idx = Map.find proc.Ssa.blocks idx

let rec handle_block live_regs blocks proc label =
  let open Result.Let_syntax in
  match find_block proc label with
  | Some block ->
     let succs =
       Set.of_list (module Ir.Label) (Ssa.successors block.Ssa.jump) in
     let%map (blocks, live_regs) =
       Set.fold succs ~init:(Ok (blocks, live_regs)) ~f:(fun acc label ->
           let%bind (blocks, live_regs) = acc in
           handle_block live_regs blocks proc label
         ) in
     let live_at_jump =
       Set.of_list (module Ir.Register) (regs_of_jump block.Ssa.jump) in
     let ending_at_jump = Set.diff live_at_jump live_regs in
     let live_regs = Set.union live_regs ending_at_jump in
     let instrs, live_regs = handle_instrs live_regs block.Ssa.instrs in
     let live_regs =
       (* Add the basic block parameters to the set of live registers *)
       List.fold block.Ssa.params ~init:live_regs ~f:(fun acc param ->
           Set.add acc param
         ) in
     let block' =
       { Ssa2.params = block.Ssa.params
       ; preds = block.Ssa.preds
       ; instrs
       ; jump = block.Ssa.jump
       ; ending_at_jump } in
     (Map.set blocks ~key:label ~data:block', live_regs)
  | None -> Message.unreachable "Unknown label"

let handle_proc proc =
  let open Result.Let_syntax in
  let live_regs = Set.empty (module Ir.Register) in
  let map = Map.empty (module Ir.Label) in
  let%map blocks, _ = handle_block live_regs map proc proc.Ssa.entry in
  { Ssa2.free_vars = proc.Ssa.free_vars
  ; params = proc.Ssa.params
  ; entry = proc.Ssa.entry
  ; blocks = blocks
  ; before_returns = proc.Ssa.before_returns
  ; return = proc.Ssa.return }

let handle_file { Ssa.procs; main } =
  let open Result.Let_syntax in
  let%bind procs =
    Map.fold procs ~init:(Ok (Map.empty (module Int)))
      ~f:(fun ~key:id ~data:proc acc ->
        let%bind map = acc in
        let%map proc = handle_proc proc in
        Map.set map ~key:id ~data:proc
      ) in
  let%map main = handle_proc main in
  { Ssa2.procs; main }
