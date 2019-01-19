open Base

type t = {
    mutable color_gen : int; (** The color to use if there are no free colors *)
    coloring : (int, int) Hashtbl.t; (** Map from registers to colors *)
    mutable free_colors : (int, Int.comparator_witness) Set.t;
      (** The colors that may be reused *)
    live_regs : (int, int) Hashtbl.t;
    visited_blocks : (Ssa.Label.t, t) Hashtbl.t;
  }

let fresh_color ctx =
  match Set.nth ctx.free_colors 0 with
  | None ->
     let c = ctx.color_gen in
     ctx.color_gen <- c + 1;
     c
  | Some color ->
     ctx.free_colors <- Set.remove_index ctx.free_colors 0;
     color

let recycle_color ctx color =
  ctx.free_colors <- Set.add ctx.free_colors color

let alloc_reg ctx reg =
  let color = fresh_color ctx in
  Hashtbl.set ctx.coloring ~key:reg ~data:color;
  Hashtbl.set ctx.live_regs ~key:reg ~data:color

let handle_ending_regs ctx regs =
  let open Result.Monad_infix in
  Set.fold regs ~init:(Ok ()) ~f:(fun acc reg ->
      acc >>= fun () ->
      match Hashtbl.find_and_remove ctx.live_regs reg with
      | None ->
         Message.unreachable ("Color unknown register " ^ (Int.to_string reg))
      | Some color -> Ok (recycle_color ctx color)
    )

let handle_instr ctx instr =
  let open Result.Monad_infix in
  handle_ending_regs ctx instr.Post_ssa.ending_regs >>| fun () ->
  match instr.Post_ssa.dest with
  | None -> ()
  | Some reg -> alloc_reg ctx reg

let handle_instrs ctx =
  let open Result.Monad_infix in
  List.fold ~init:(Ok ()) ~f:(fun acc instr ->
      acc >>= fun () ->
      handle_instr ctx instr
    )

let rec handle_block ctx proc label =
  let open Result.Monad_infix in
  match Map.find proc.Post_ssa.blocks label with
  | None -> Message.unreachable "Unknown block"
  | Some block ->
     (* next_color is either the greatest color of all the predecessor blocks
        or None if not all predecessors have been visited *)
     let opt =
       Set.fold block.Post_ssa.preds
         ~init:(Some (ctx.free_colors, ctx.color_gen))
         ~f:(fun opt label ->
           Option.bind opt
             ~f:(fun (free_colors, next_color) ->
               Option.map (Hashtbl.find ctx.visited_blocks label)
                 ~f:(fun block_data ->
                   let f ~key:_ a _ = Hashtbl.Set_to a in
                   Hashtbl.merge_into
                     ~src:block_data.live_regs ~dst:ctx.live_regs ~f;
                   ( Set.union free_colors block_data.free_colors
                   , if block_data.color_gen > next_color then
                       block_data.color_gen
                     else
                       next_color )
                 )
             )
         ) in
     match opt with
     | None -> Ok () (* Not all predecessors have been visited, return *)
     | Some (free_colors, color_gen) ->
        let ctx = { ctx with color_gen; free_colors } in
        match Hashtbl.add ctx.visited_blocks ~key:label ~data:ctx with
        | `Duplicate -> Ok ()
        | `Ok ->
           handle_instrs ctx block.Post_ssa.instrs >>= fun () ->
           handle_ending_regs ctx block.Post_ssa.ending_at_jump >>= fun () ->
           let succs = Ssa.successors block.Post_ssa.jump in
           List.fold succs ~init:(Ok ()) ~f:(fun acc label ->
               (* Use a physically distinct state *)
               let ctx = { ctx with live_regs = Hashtbl.create (module Int) } in
               acc >>= fun () ->
               handle_block ctx proc label
             )

let handle_proc proc =
  let open Result.Monad_infix in
  let ctx =
    { color_gen = 0
    ; coloring = Hashtbl.create (module Int)
    ; free_colors = Set.empty (module Int)
    ; live_regs = Hashtbl.create (module Int)
    ; visited_blocks = Hashtbl.create (module Int) } in
  (* Perform register allocation on free variables and parameters first *)
  List.iter proc.Post_ssa.free_vars ~f:(fun reg -> alloc_reg ctx reg);
  List.iter proc.Post_ssa.params ~f:(fun reg -> alloc_reg ctx reg);
  (* Perform register allocation on entry block; blocks that are unreachable
     will not be visited. *)
  handle_block ctx proc proc.Post_ssa.entry >>| fun () ->
  ctx.coloring

let handle_package package =
  let open Result.Monad_infix in
  Map.fold package.Post_ssa.procs ~init:(Ok (Map.empty (module Int)))
    ~f:(fun ~key ~data acc ->
      acc >>= fun map ->
      handle_proc data >>| fun coloring ->
      Map.set map ~key ~data:coloring
    ) >>= fun map ->
  handle_proc package.Post_ssa.main
  >>| fun main's_coloring ->
  (map, main's_coloring)
