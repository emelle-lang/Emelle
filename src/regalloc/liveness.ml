open Base

let operands_of_opcode = function
  | Ssa.Assign(lval, rval) -> [lval; rval]
  | Ssa.Box list -> list
  | Ssa.Box_dummy _ -> []
  | Ssa.Call(f, arg, args) -> f::arg::args
  | Ssa.Deref op -> [op]
  | Ssa.Fun(_, captures) -> captures
  | Ssa.Get(op, _) -> [op]
  | Ssa.Load op -> [op]
  | Ssa.Memcopy(dest, src) -> [dest; src]
  | Ssa.Phi _ -> []
  | Ssa.Prim _ -> []
  | Ssa.Ref op -> [op]

let operand_of_jump = function
  | Ssa.Switch(scrut, _, _) -> Some scrut
  | _ -> None

let regs_of_opcode opcode =
  let operands = operands_of_opcode opcode in
  List.fold operands ~init:[] ~f:(fun acc ->
      function
      | Anf.Register reg -> reg::acc
      | _ -> acc
    )

let reg_of_jump jump =
  let open Option.Monad_infix in
  operand_of_jump jump >>= function
  | Anf.Register reg -> Some reg
  | _ -> None

let handle_regs live_regs regs =
  let regs = Set.of_list (module Int) regs in
  let new_regs = Set.diff regs live_regs in
  (Set.union live_regs regs, new_regs)

let handle_instr live_regs instr =
  let (live_regs, ending_regs) =
    handle_regs live_regs (regs_of_opcode instr.Ssa.opcode) in
  let live_regs =
    match instr.Ssa.dest with
    | None -> live_regs
    | Some reg -> Set.remove live_regs reg in
  ( { Post_ssa.dest = instr.Ssa.dest
    ; opcode = instr.Ssa.opcode
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

let handle_phi_elems live_regs elems =
  let regs =
    Array.fold elems ~init:[] ~f:(fun acc operand ->
        match operand with
        | Anf.Register reg -> reg::acc
        | _ -> acc
      )
  in handle_regs live_regs regs

let find_block proc idx = Map.find proc.Ssa.blocks idx

let rec handle_block live_regs blocks proc label =
  let open Result.Monad_infix in
  match find_block proc label with
  | Some block ->
     let succs = Ssa.successors block.Ssa.jump in
     List.fold succs ~init:(Ok (blocks, live_regs)) ~f:(fun acc label ->
         acc >>= fun (blocks, live_regs) ->
         handle_block live_regs blocks proc label
       ) >>| fun (blocks, live_regs) ->
     let instrs, live_regs = handle_instrs live_regs block.Ssa.instrs in
     let block' =
       { Post_ssa.preds = block.Ssa.preds; instrs; jump = block.Ssa.jump } in
     (Map.set blocks ~key:label ~data:block', live_regs)
  | None -> Message.unreachable "Unknown label"

let handle_proc proc =
  let open Result.Monad_infix in
  let live_regs =
    match proc.Ssa.return with
    | Anf.Register reg ->
       Set.singleton (module Int) reg
    | _ -> Set.empty (module Int) in
  let map = Map.empty (module Int) in
  handle_block live_regs map proc proc.Ssa.entry
  >>| fun (blocks, _) ->
  { Post_ssa.params = proc.Ssa.params
  ; entry = proc.Ssa.entry
  ; blocks = blocks
  ; before_return = proc.Ssa.before_return
  ; return = proc.Ssa.return }

let handle_package { Ssa.procs; main } =
  let open Result.Monad_infix in
  Map.fold procs ~init:(Ok (Map.empty (module Int)))
    ~f:(fun ~key:id ~data:proc acc ->
      acc >>= fun map ->
      handle_proc proc >>| fun proc ->
      Map.set map ~key:id ~data:proc
    ) >>= fun procs ->
  handle_proc main >>| fun main ->
  { Post_ssa.procs; main }
