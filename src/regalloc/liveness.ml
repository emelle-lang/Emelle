open Base

let operands_of_opcode = function
  | Ssa.Assign(dest, lval, rval) -> Some dest, [lval; rval]
  | Ssa.Box(dest, _, list) -> Some dest, list
  | Ssa.Box_dummy (dest, _) -> Some dest, []
  | Ssa.Call(dest, f, arg, args) -> Some dest, f::arg::args
  | Ssa.Deref(dest, op) -> Some dest, [op]
  | Ssa.Get(dest, op, _) -> Some dest, [op]
  | Ssa.Load(dest, op) -> Some dest, [op]
  | Ssa.Memcopy(dest, src) -> None, [dest; src]
  | Ssa.Phi(dest, _) -> Some dest, []
  | Ssa.Prim(dest, _) -> Some dest, []
  | Ssa.Ref(dest, op) -> Some dest, [op]
  | Ssa.Tag(dest, op) -> Some dest, [op]

let operands_of_jump = function
  | Ssa.Break(_, args) -> args
  | Ssa.Fail -> []
  | Ssa.Return operand -> [operand]
  | Ssa.Switch(scrut, _, _) -> [scrut]

let regs_of_opcode opcode =
  let dest_opt, operands = operands_of_opcode opcode in
  ( dest_opt
  , List.fold operands ~init:[] ~f:(fun acc ->
        function
        | Ir.Operand.Register reg -> reg::acc
        | _ -> acc
  ) )

let regs_of_jump jump =
  let open List.Monad_infix in
  operands_of_jump jump >>= function
  | Ir.Operand.Register reg -> [reg]
  | _ -> []

let handle_regs live_regs regs =
  let regs = Set.of_list (module Ir.Register) regs in
  let new_regs = Set.diff regs live_regs in
  (Set.union live_regs regs, new_regs)

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
  let open Result.Monad_infix in
  match find_block proc label with
  | Some block ->
     let succs =
       Set.of_list (module Ir.Label) (Ssa.successors block.Ssa.jump) in
     Set.fold succs ~init:(Ok (blocks, live_regs)) ~f:(fun acc label ->
         acc >>= fun (blocks, live_regs) ->
         handle_block live_regs blocks proc label
       ) >>| fun (blocks, live_regs) ->
     let live_at_jump =
       Set.of_list (module Ir.Register) (regs_of_jump block.Ssa.jump) in
     let ending_at_jump = Set.diff live_at_jump live_regs in
     let live_regs = Set.union live_regs ending_at_jump in
     let instrs, live_regs = handle_instrs live_regs block.Ssa.instrs in
     let block' =
       { Ssa2.preds = block.Ssa.preds
       ; instrs
       ; jump = block.Ssa.jump
       ; ending_at_jump } in
     (Map.set blocks ~key:label ~data:block', live_regs)
  | None -> Message.unreachable "Unknown label"

let handle_proc proc =
  let open Result.Monad_infix in
  let live_regs = Set.empty (module Ir.Register) in
  let map = Map.empty (module Ir.Label) in
  handle_block live_regs map proc proc.Ssa.entry
  >>| fun (blocks, _) ->
  { Ssa2.free_vars = proc.Ssa.free_vars
  ; params = proc.Ssa.params
  ; entry = proc.Ssa.entry
  ; blocks = blocks
  ; before_return = proc.Ssa.before_return }

let handle_package { Ssa.procs; main } =
  let open Result.Monad_infix in
  Map.fold procs ~init:(Ok (Map.empty (module Int)))
    ~f:(fun ~key:id ~data:proc acc ->
      acc >>= fun map ->
      handle_proc proc >>| fun proc ->
      Map.set map ~key:id ~data:proc
    ) >>= fun procs ->
  handle_proc main >>| fun main ->
  { Ssa2.procs; main }
