(** This module replaces virtual registers with stack offsets and removes phi
    nodes. *)

open Base

let compile_operand coloring = function
  | Ir.Operand.Extern_var path -> Ok (Asm.Extern_var path)
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
      operand::operands)

type compile_opcode_result =
  | Opcode of Asm.instr
  | Phi of int

let compile_instr coloring { Ssa2.dest; opcode; _ } =
  let open Result.Let_syntax in
  match Hashtbl.find coloring.Color.map dest with
  | None -> Message.unreachable "compile_instr"
  | Some dest ->
     match opcode with
     | Ssa.Assign(lval, rval) ->
        let%bind lval = compile_operand coloring lval in
        let%map rval = compile_operand coloring rval in
        Opcode (Asm.Assign(dest, lval, rval))
     | Ssa.Box operands ->
        let%map operands = compile_operands coloring operands in
        Opcode (Asm.Box(dest, operands))
     | Ssa.Box_dummy i -> Ok (Opcode (Asm.Box_dummy(dest, i)))
     | Ssa.Call(f, arg, args) ->
        let%bind f = compile_operand coloring f in
        let%bind arg = compile_operand coloring arg in
        let%map args = compile_operands coloring args in
        Opcode (Asm.Call(dest, f, arg, args))
     | Ssa.Deref operand ->
        let%map operand = compile_operand coloring operand in
        Opcode (Asm.Deref(dest, operand))
     | Ssa.Fun(f_idx, captures) ->
        let%map captures = compile_operands coloring captures in
        Opcode (Asm.Fun(dest, f_idx, captures))
     | Ssa.Get(operand, idx) ->
        let%map operand = compile_operand coloring operand in
        Opcode (Asm.Get(dest, operand, idx))
     | Ssa.Load operand ->
        let%map operand = compile_operand coloring operand in
        Opcode (Asm.Move(dest, operand))
     | Ssa.Memcopy(mem_dest, src) ->
        let%bind mem_dest = compile_operand coloring mem_dest in
        let%map src = compile_operand coloring src in
        Opcode (Asm.Memcopy(dest, mem_dest, src))
     | Ssa.Phi idx -> Ok (Phi idx)
     | Ssa.Prim str -> Ok (Opcode (Asm.Prim(dest, str)))
     | Ssa.Ref operand ->
        let%map operand = compile_operand coloring operand in
        Opcode (Asm.Ref(dest, operand))

let rec compile_basic_block new_blocks coloring proc label =
  let open Result.Let_syntax in
  match Map.find new_blocks label with
  | Some { Asm.phis; _ } -> Ok (new_blocks, phis)
  | None ->
     match Map.find proc.Ssa2.blocks label with
     | None -> Message.unreachable "to_asm compile_basic_block 1"
     | Some block ->
        let instrs = Queue.create () in
        let phis = Queue.create () in
        let%bind () =
          List.fold block.Ssa2.instrs ~init:(Ok ()) ~f:(fun acc instr ->
              let%bind () = acc in
              match Hashtbl.find coloring.Color.map instr.Ssa2.dest with
              | None -> Message.unreachable "to_asm compile_basic_block 2"
              | Some color ->
                 match%map compile_instr coloring instr with
                 | Opcode instr -> Queue.enqueue instrs instr
                 | Phi idx -> Queue.enqueue phis (color, idx) ) in
        let%map new_blocks = match block.Ssa2.jump with
          | Ssa.Break(label, args) ->
             let args = Array.of_list args in
             let%bind new_blocks, phis =
               compile_basic_block new_blocks coloring proc label in
             let%map () =
               Queue.fold phis ~init:(Ok ()) ~f:(fun acc (color, idx) ->
                   let%bind () = acc in
                   let%map operand = compile_operand coloring (args.(idx)) in
                   Queue.enqueue instrs (Asm.Move(color, operand))) in
             Queue.enqueue instrs (Asm.Break label);
             new_blocks
          | Ssa.Fail ->
             Queue.enqueue instrs (Asm.Fail);
             Ok new_blocks
          | Ssa.Return operand ->
             let%map operand = compile_operand coloring operand in
             Queue.enqueue instrs (Asm.Return operand);
             new_blocks
          | Ssa.Switch(scrut, cases, else_case) ->
             let%bind scrut = compile_operand coloring scrut in
             Queue.enqueue instrs (Asm.Switch(scrut, cases, else_case));
             let%bind new_blocks =
               List.fold cases ~init:(Ok new_blocks) ~f:(fun acc (_, label) ->
                   let%bind new_blocks = acc in
                   let%map new_blocks, _ =
                     compile_basic_block new_blocks coloring proc label
                   in new_blocks
                 ) in
             let%map new_blocks, _ =
               compile_basic_block new_blocks coloring proc else_case
             in new_blocks
        in (Map.set new_blocks ~key:label ~data:{ Asm.instrs; phis}, phis)

let compile_proc coloring proc =
  let open Result.Let_syntax in
  let map = Map.empty (module Ir.Label) in
  let%bind free_vars =
    List.fold_right proc.Ssa2.free_vars ~init:(Ok []) ~f:(fun reg acc ->
        let%bind list = acc in
        match Hashtbl.find coloring.Color.map reg with
        | Some color -> Ok (color::list)
        | None -> Message.unreachable "compile_proc free_vars"
      ) in
  let%bind params =
    List.fold_right proc.Ssa2.params ~init:(Ok []) ~f:(fun reg acc ->
        let%bind list = acc in
        match Hashtbl.find coloring.Color.map reg with
        | Some color -> Ok (color::list)
        | None -> Message.unreachable "compile_proc params"
      ) in
  let%map blocks, _ = compile_basic_block map coloring proc proc.Ssa2.entry in
  { Asm.free_vars; params; blocks; frame_size = coloring.Color.frame_size }

let compile_package { Color.colorings; main's_coloring } package =
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
