(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base

type ctx = {
    mutable color_gen : int; (** The color to use if there are no free colors *)
    coloring : (Ir.Register.t, int) Hashtbl.t;
      (** Map from registers to colors *)
    frame_size : int ref;
    mutable free_colors : (int, Int.comparator_witness) Set.t;
      (** The colors that may be reused *)
    live_regs : (Ir.Register.t, int) Hashtbl.t;
      (** The currently live registers *)
    visited_blocks : (Ir.Label.t, ctx) Hashtbl.t;
  }

type coloring = {
    map : (Ir.Register.t, int) Hashtbl.t;
    frame_size : int;
  }

type t = {
    colorings : (int, coloring, Int.comparator_witness) Map.t;
    main's_coloring : coloring;
  }

let fresh_color ctx =
  match Set.nth ctx.free_colors 0 with
  | None ->
     let c = ctx.color_gen in
     ctx.color_gen <- c + 1;
     if ctx.color_gen > !(ctx.frame_size) then (
       ctx.frame_size := ctx.color_gen
     );
     c
  | Some color ->
     ctx.free_colors <- Set.remove_index ctx.free_colors 0;
     color

let recycle_color _ctx _color = ()
  (*ctx.free_colors <- Set.add ctx.free_colors color*)

let alloc_reg ctx reg =
  let color = fresh_color ctx in
  Hashtbl.set ctx.coloring ~key:reg ~data:color;
  Hashtbl.set ctx.live_regs ~key:reg ~data:color

let recycle_ending_regs ctx regs =
  Set.fold_result regs ~init:() ~f:(fun () reg ->
      match Hashtbl.find_and_remove ctx.live_regs reg with
      | None ->
         Message.unreachable
           ("Color unknown register " ^ (Ir.Register.to_string reg))
      | Some color -> Ok (recycle_color ctx color)
    )

let handle_instr ctx instr =
  let open Result.Let_syntax in
  let%map () = recycle_ending_regs ctx instr.Ssa2.ending_regs in
  ignore (Option.map ~f:(fun dest -> alloc_reg ctx dest) instr.Ssa2.dest)

let handle_instrs ctx =
  List.fold_result ~init:() ~f:(fun () instr -> handle_instr ctx instr)

let rec handle_block ctx proc label =
  let open Result.Let_syntax in
  match Map.find proc.Ssa2.blocks label with
  | None -> Message.unreachable "Unknown block"
  | Some block ->
     (* opt is either a pair consisting of the set of free colors and the
        greatest color of all the predecessor blocks or None if not all
        predecessors have been visited *)
     let opt =
       Set.fold block.Ssa2.preds ~init:(Some (ctx.free_colors, ctx.color_gen))
         ~f:(fun acc label ->
           let open Option.Let_syntax in
           let%bind free_colors, next_color = acc in
           let%map block_data = Hashtbl.find ctx.visited_blocks label in
           let f ~key:_ a _ = Hashtbl.Set_to a in
           Hashtbl.merge_into ~src:block_data.live_regs ~dst:ctx.live_regs ~f;
           ( Set.union free_colors block_data.free_colors
           , if block_data.color_gen > next_color then
               block_data.color_gen
             else
               next_color )) in
     match opt with
     | None -> Ok () (* Not all predecessors have been visited, return *)
     | Some (free_colors, color_gen) ->
        let ctx = { ctx with color_gen; free_colors } in
        match Hashtbl.add ctx.visited_blocks ~key:label ~data:ctx with
        | `Duplicate -> Ok ()
        | `Ok ->
           List.iter block.Ssa2.params ~f:(fun reg_param ->
               alloc_reg ctx reg_param
             );
           let%bind () = handle_instrs ctx block.Ssa2.instrs in
           (* This code is suspicious; I haven't triggered a bug but I think it
              is incorrect. I don't think it's safe to recycle the registers at
              the end of a basic block at this point because the successor
              blocks needs to receive them. I think that the parameters of the
              successor blocks need to be allocated first. *)
           let%bind () = recycle_ending_regs ctx block.Ssa2.ending_at_jump in
           let succs = Ssa.successors block.Ssa2.jump in
           List.fold_result succs ~init:() ~f:(fun () label ->
               (* Use a physically distinct state *)
               let ctx =
                 { ctx with live_regs = Hashtbl.create (module Ir.Register) }
               in handle_block ctx proc label
             )

let handle_proc proc =
  let open Result.Let_syntax in
  let ctx =
    { color_gen = 0
    ; coloring = Hashtbl.create (module Ir.Register)
    ; frame_size = ref 0
    ; free_colors = Set.empty (module Int)
    ; live_regs = Hashtbl.create (module Ir.Register)
    ; visited_blocks = Hashtbl.create (module Ir.Label) } in
  (* Perform register allocation on the return register, free variables and
     parameters first *)
  alloc_reg ctx proc.Ssa2.return;
  List.iter proc.Ssa2.free_vars ~f:(fun reg -> alloc_reg ctx reg);
  List.iter proc.Ssa2.params ~f:(fun reg -> alloc_reg ctx reg);
  (* Perform register allocation on entry block; blocks that are unreachable
     will not be visited. *)
  let%map () = handle_block ctx proc proc.Ssa2.entry in
  { map = ctx.coloring; frame_size = !(ctx.frame_size) }

let handle_file package =
  let open Result.Let_syntax in
  Map.fold package.Ssa2.procs ~init:(Ok (Map.empty (module Int)))
    ~f:(fun ~key ~data acc ->
      let%bind map = acc in
      let%map coloring = handle_proc data in
      Map.set map ~key ~data:coloring
    ) >>= fun colorings ->
  let%map main's_coloring = handle_proc package.Ssa2.main in
  { colorings; main's_coloring }
