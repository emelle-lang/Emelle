(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

(** This module replaces virtual registers with stack offsets and eliminates
    basic block parameters. *)

open Base

let compile_operand coloring = function
  | Ir.Operand.Lit lit -> Ok (Asm.Lit lit)
  | Ir.Operand.Register reg ->
     match Hashtbl.find coloring.Color.map reg with
     | Some color -> Ok (Asm.Stack color)
     | None -> Message.unreachable "to_asm compile_operand"

let compile_operands coloring =
  let open Result.Let_syntax in
  List.fold_right ~init:(Ok []) ~f:(fun operand acc ->
      let%bind operands = acc in
      let%map operand = compile_operand coloring operand in
      operand :: operands)

let find_color coloring dest =
  match Hashtbl.find coloring.Color.map dest with
  | None -> Message.unreachable "find_color compile_basic_block"
  | Some color -> Ok color

let compile_instr coloring opcode =
  let open Result.Let_syntax in
  match opcode with
  | Ssa.Assign(dest, lval, rval) ->
     let%bind dest = find_color coloring dest in
     let%bind lval = compile_operand coloring lval in
     let%map rval = compile_operand coloring rval in
     Asm.Assign(dest, lval, rval)
  | Ssa.Box(dest, tag, operands) ->
     let%bind dest = find_color coloring dest in
     let%map operands = compile_operands coloring operands in
     Asm.Box(dest, tag, operands)
  | Ssa.Box_dummy(dest, i) ->
     let%bind dest = find_color coloring dest in
     Ok (Asm.Box_dummy(dest, i))
  | Ssa.Call(dest, f, arg, args) ->
     let%bind dest = find_color coloring dest in
     let%bind f = compile_operand coloring f in
     let%bind arg = compile_operand coloring arg in
     let%map args = compile_operands coloring args in
     Asm.Call(dest, f, arg, args)
  | Ssa.Deref(dest, operand) ->
     let%bind dest = find_color coloring dest in
     let%map operand = compile_operand coloring operand in
     Asm.Deref(dest, operand)
  | Ssa.Get(dest, operand, idx) ->
     let%bind dest = find_color coloring dest in
     let%map operand = compile_operand coloring operand in
     Asm.Get(dest, operand, idx)
  | Ssa.Load(dest, operand) ->
     let%bind dest = find_color coloring dest in
     let%map operand = compile_operand coloring operand in
     Asm.Move(dest, operand)
  | Ssa.Package(dest, str) ->
     let%map dest = find_color coloring dest in
     Asm.Package(dest, str)
  | Ssa.Prim(dest, str) ->
     let%map dest = find_color coloring dest in
     Asm.Prim(dest, str)
  | Ssa.Ref(dest, operand) ->
     let%bind dest = find_color coloring dest in
     let%map operand = compile_operand coloring operand in
     Asm.Ref(dest, operand)
  | Ssa.Set_field(dest, idx, op) ->
     let%bind dest = compile_operand coloring dest in
     let%map op = compile_operand coloring op in
     Asm.Set_field(dest, idx, op)
  | Ssa.Set_tag(dest, tag) ->
     let%map dest = compile_operand coloring dest in
     Asm.Set_tag(dest, tag)
  | Ssa.Tag(dest, operand) ->
     let%bind dest = find_color coloring dest in
     let%map operand = compile_operand coloring operand in
     Asm.Tag(dest, operand)

let rec compile_basic_block new_blocks coloring proc label =
  let open Result.Let_syntax in
  match Map.find new_blocks label with
  | Some asm_block -> Ok (new_blocks, asm_block)
  | None ->
     match Map.find proc.Ssa2.blocks label with
     | None -> Message.unreachable "to_asm compile_basic_block 1"
     | Some block ->
        let instrs = Queue.create () in
        let%bind params =
          List.fold_right block.Ssa2.params ~init:(Ok [])
            ~f:(fun reg_param acc ->
              let%bind list = acc in
              let%map color = find_color coloring reg_param in
              color :: list
            ) in
        let%bind () =
          List.fold_result block.Ssa2.instrs ~init:() ~f:(fun () instr ->
              let%map instr = compile_instr coloring instr.Ssa2.opcode in
              Queue.enqueue instrs instr
            ) in
        let%map new_blocks = match block.Ssa2.jump with
          | Ssa.Break(label, args) ->
             let args = Array.of_list args in
             let%bind new_blocks, asm_block =
               compile_basic_block new_blocks coloring proc label in
             let%map _ =
               List.fold_result asm_block.Asm.block_params ~init:0
                 ~f:(fun idx color ->
                   let%map operand = compile_operand coloring (args.(idx)) in
                   Queue.enqueue instrs (Asm.Move(color, operand));
                   idx + 1
                 ) in
             Queue.enqueue instrs (Asm.Break label);
             new_blocks
          | Ssa.Fail ->
             Queue.enqueue instrs Asm.Fail;
             Ok new_blocks
          | Ssa.Return ->
             Queue.enqueue instrs Asm.Return;
             Ok new_blocks
          | Ssa.Switch(scrut, cases, else_case) ->
             let%bind scrut = compile_operand coloring scrut in
             Queue.enqueue instrs (Asm.Switch(scrut, cases, else_case));
             let%bind new_blocks =
               List.fold_result cases ~init:new_blocks
                 ~f:(fun new_blocks (_, label) ->
                   let%map new_blocks, _ =
                     compile_basic_block new_blocks coloring proc label
                   in new_blocks
                 ) in
             let%map new_blocks, _ =
               compile_basic_block new_blocks coloring proc else_case
             in new_blocks
          | Ssa.Tail_call(f, arg, args) ->
             let%bind f = compile_operand coloring f in
             let%bind arg = compile_operand coloring arg in
             let%map args = compile_operands coloring args in
             Queue.enqueue instrs (Asm.Tail_call(f, arg, args));
             new_blocks
        in
        let new_block = { Asm.instrs; block_params = params } in
        (Map.set new_blocks ~key:label ~data:new_block, new_block)

let compile_proc coloring proc =
  let open Result.Let_syntax in
  let map = Map.empty (module Ir.Label) in
  let%bind free_vars =
    List.fold_right proc.Ssa2.free_vars ~init:(Ok []) ~f:(fun reg acc ->
        let%bind list = acc in
        match Hashtbl.find coloring.Color.map reg with
        | Some color -> Ok (color :: list)
        | None -> Message.unreachable "compile_proc free_vars"
      ) in
  let%bind params =
    List.fold_right proc.Ssa2.params ~init:(Ok []) ~f:(fun reg acc ->
        let%bind list = acc in
        match Hashtbl.find coloring.Color.map reg with
        | Some color -> Ok (color :: list)
        | None -> Message.unreachable "compile_proc params 1"
      ) in
  let%bind blocks, _ = compile_basic_block map coloring proc proc.Ssa2.entry in
  match Hashtbl.find coloring.Color.map proc.Ssa2.return with
  | Some color ->
     Ok { Asm.free_vars
        ; params
        ; entry = proc.Ssa2.entry
        ; blocks
        ; frame_size = coloring.Color.frame_size
        ; return = color }
  | None -> Message.unreachable "compile_proc params 2"

let compile { Color.colorings; main's_coloring } package =
  let open Result.Let_syntax in
  let%bind procs =
    Map.fold package.Ssa2.procs ~init:(Ok (Map.empty (module Int)))
      ~f:(fun ~key:name ~data:proc acc ->
        let%bind procs = acc in
        match Map.find colorings name with
        | None -> Message.unreachable "to_asm compile_package"
        | Some coloring ->
           let%map blocks = compile_proc coloring proc in
           Map.set procs ~key:name ~data:blocks
      ) in
  let%map main = compile_proc main's_coloring package.Ssa2.main in
  { Asm.procs; main }
