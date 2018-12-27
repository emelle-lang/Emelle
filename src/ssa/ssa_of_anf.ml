(** This transformation turns ANF join points, which are local to the case expr,
    into SSA basic blocks, which are local to the function. Here, the compiler
    compiles decision trees into switches, jumps, and phi nodes. *)

open Base

type t = {
    procs : (int, Ssa.proc, Int.comparator_witness) Map.t ref;
    blocks : (int, Ssa.basic_block, Int.comparator_witness) Map.t ref;
    label_gen : int ref;
    proc_gen : int ref;
    instrs : Ssa.instr Queue.t;
    curr_block : Ssa.Label.t;
    cont : Ssa.cont;
  }

(** [fresh_block ctx ~cont] applies [cont] to the index of the next basic block,
    [cont] returns a tuple consisting of a result, the block predecessors, the
    instruction queue, and the block successor. The entire function returns
    [cont]'s result *)
let fresh_block ctx ~cont =
  let open Result.Let_syntax in
  let idx = !(ctx.label_gen) in
  ctx.label_gen := idx + 1;
  let%map ret, preds, instrs, tail = cont idx in
  let block = { Ssa.instrs; preds; tail } in
  ctx.blocks := Map.set !(ctx.blocks) ~key:idx ~data:block;
  (ret, block)

let rec compile_decision_tree ctx instrs origin_label cont_idx branches =
  let open Result.Let_syntax in
  function
  | Anf.Deref(occ, dest, tree) ->
     Queue.enqueue instrs { Ssa.dest = Some dest; opcode = Deref occ };
     compile_decision_tree ctx instrs origin_label cont_idx branches tree
  | Anf.Fail -> Ok Ssa.Fail
  | Anf.Leaf(operands, idx) ->
     let branch_idx, _, block = branches.(idx) in
     block.Ssa.preds <-
       Map.set block.Ssa.preds
         ~key:(Ssa.Label.Block cont_idx) ~data:(Array.of_list operands);
     Ok (Ssa.Break (Ssa.Label.Block branch_idx))
  | Anf.Switch(tag_reg, occ, trees, else_tree) ->
     Queue.enqueue instrs { Ssa.dest = Some tag_reg; opcode = Get(occ, 0) };
     let%bind cases =
       Map.fold trees ~init:(Ok []) ~f: begin
           fun ~key:case ~data:(regs, tree) acc ->
           let%bind list = acc in
           let%map (result, _) =
             fresh_block ctx ~cont:
               begin
                 fun jump_idx ->
                 let jump_instrs = Queue.create () in
                 let%map cont =
                   List.iteri ~f:(fun idx reg ->
                       Queue.enqueue jump_instrs
                         { Ssa.dest = Some reg; opcode = Get(occ, idx + 1) }
                     ) regs;
                   compile_decision_tree ctx jump_instrs
                     (Ssa.Label.Block jump_idx) jump_idx branches tree
                 in
                 ( (case, Ssa.Label.Block jump_idx)::list
                 , Map.singleton (module Ssa.Label) origin_label [||]
                 , jump_instrs
                 , cont )
               end
           in result
         end in
     let%map (else_block_idx, _) =
       fresh_block ctx ~cont:(fun else_idx ->
         let else_instrs = Queue.create () in
         let%map cont =
           compile_decision_tree ctx else_instrs
             (Ssa.Label.Block else_idx) else_idx branches else_tree
         in
         ( else_idx
         , Map.singleton (module Ssa.Label) origin_label [||]
         , else_instrs
         , cont )
       )
     in Ssa.Switch(Anf.Register tag_reg, cases, Ssa.Label.Block else_block_idx)

let rec compile_opcode ctx anf ~cont
        : ( Ssa.Label.t * Ssa.cont * Ssa.operand
          , Message.error Sequence.t ) Result.t =
  let open Result.Let_syntax in
  match anf with
  | Anf.Assign(lval, rval) -> cont ctx (Ssa.Assign(lval, rval))
  | Anf.Box contents -> cont ctx (Ssa.Box contents)
  | Anf.Call(f, arg, args) -> cont ctx (Ssa.Call(f, arg, args))
  | Anf.Case(tree, join_points) ->
     let%map result, _ =
       (* The basic block for the case expr continuation *)
       fresh_block ctx ~cont:(fun cont_idx ->
           let cont_instrs = Queue.create () in
           let%bind branches =
             List.fold_right join_points ~init:(Ok [])
               ~f:(fun (reg_args, instr) acc ->
                 let%bind list = acc in
                 (* The basic block for the join point *)
                 let%map (branch_idx, exit_phi_elem), block =
                   fresh_block ctx ~cont:(fun branch_idx ->
                       let branch_instrs = Queue.create () in
                       List.iteri reg_args ~f:(fun i reg_arg ->
                           Queue.enqueue branch_instrs
                             { Ssa.dest = Some reg_arg
                             ; opcode = Phi i };
                         );
                       let%map _, tail, exit_phi_elem =
                         compile_instr
                           { ctx with
                             instrs = branch_instrs
                           ; curr_block = Ssa.Label.Block branch_idx
                           ; cont = Ssa.Break (Ssa.Label.Block cont_idx) }
                           instr in
                       ( (branch_idx, exit_phi_elem)
                       , Map.empty (module Ssa.Label)
                       , branch_instrs
                       , tail )
                     )
                 in (branch_idx, exit_phi_elem, block)::list
               ) in
           let%bind cont_from_decision_tree =
             compile_decision_tree ctx ctx.instrs ctx.curr_block cont_idx
               (Array.of_list branches) tree in
           let preds =
             List.fold branches ~init:(Map.empty (module Ssa.Label))
               ~f:(fun acc (idx, exit_phi_elem, _) ->
                 Map.set acc ~key:(Ssa.Label.Block idx) ~data:[|exit_phi_elem|]
               ) in
           (* Label of continuation block, tail of continuation block *)
           let%map (label, tail, result) =
             (* Compile the rest of the ANF in the continuation block *)
             cont
               { ctx with
                 instrs = cont_instrs
               ; curr_block = Ssa.Label.Block cont_idx }
               (Ssa.Phi 0) in
           (* Cont for origin block, cont for continuation block *)
           ( (label, cont_from_decision_tree, result)
           , preds
           , cont_instrs
           , tail )
         ) in result
  | Anf.Fun proc ->
     let env = proc.Anf.env in
     let%bind proc = compile_proc ctx proc in
     let idx = !(ctx.proc_gen) in
     ctx.proc_gen := idx + 1;
     let procs = Map.set !(ctx.procs) ~key:idx ~data:proc in
     ctx.procs := procs;
     cont ctx (Ssa.Fun(idx, env))
  | Anf.Load o -> cont ctx (Ssa.Load o)
  | Anf.Prim p -> cont ctx (Ssa.Prim p)
  | Anf.Ref x -> cont ctx (Ssa.Ref x)

and compile_instr ctx anf
    : ( Ssa.Label.t * Ssa.cont * Ssa.operand
      , Message.error Sequence.t ) Result.t =
  let open Result.Let_syntax in
  match anf.Anf.instr with
  | Anf.Let(reg, op, next) ->
     compile_opcode ctx op ~cont:(fun ctx op ->
         Queue.enqueue ctx.instrs { Ssa.dest = Some reg; opcode = op };
         compile_instr ctx next
       )
  | Anf.Let_rec(bindings, next) ->
     (* Initialize registers with dummy allocations *)
     let%bind () =
       List.fold_left bindings ~init:(Ok ()) ~f:begin
           fun acc (reg, _, def) ->
           let%bind () = acc in
           let%map size =
             begin match def with
             | Anf.Box list -> Ok (List.length list)
             | Anf.Fun proc -> Ok (List.length proc.Anf.env + 1)
             | _ -> Error (Sequence.return Message.Unsafe_let_rec)
             end in
           Queue.enqueue ctx.instrs
             { Ssa.dest = Some reg; opcode = Ssa.Box_dummy size }
         end in
     (* Accumulator is a function *)
     List.fold_right bindings ~init:(fun ctx ->
         compile_instr ctx next
       ) ~f:(fun (reg, temp, op) acc ctx ->
         compile_opcode ctx op ~cont:(fun ctx op ->
             (* Evaluate the let-rec binding RHS *)
             Queue.enqueue ctx.instrs { Ssa.dest = Some temp; opcode = op };
             (* Mutate the memory that the register points to *)
             Queue.enqueue ctx.instrs
               { Ssa.dest = None
               ; opcode = Ssa.Memcopy(Anf.Register reg, Anf.Register temp) };
             acc ctx
           )
       ) ctx
  | Anf.Seq(op, next) ->
     compile_opcode ctx op ~cont:(fun ctx opcode ->
         Queue.enqueue ctx.instrs { Ssa.dest = None; opcode = opcode };
         compile_instr ctx next
       )
  | Anf.Break operand -> Ok (ctx.curr_block, ctx.cont, operand)

and compile_proc ctx proc =
  let open Result.Let_syntax in
  let blocks = ref (Map.empty (module Int)) in
  let instrs = Queue.create () in
  let state =
    { ctx with
      blocks
    ; instrs
    ; label_gen = ref 0
    ; curr_block = Ssa.Label.Entry
    ; cont = Ssa.Return } in
  let%map before_return, tail, return = compile_instr state proc.Anf.body in
  { Ssa.params = proc.Anf.params
  ; blocks = !blocks
  ; entry = { preds = Map.empty (module Ssa.Label); instrs; tail }
  ; before_return
  ; return }

let compile_module anf =
  let open Result.Let_syntax in
  let ctx =
    { procs = ref (Map.empty (module Int))
    ; blocks = ref (Map.empty (module Int))
    ; instrs = Queue.create ()
    ; proc_gen = ref 0
    ; label_gen = ref 0
    ; curr_block = Ssa.Label.Entry
    ; cont = Ssa.Return } in
  let%map before_return, tail, return = compile_instr ctx anf in
  let main_proc =
    { Ssa.params = []
    ; blocks = !(ctx.blocks)
    ; entry =
        { preds = Map.empty (module Ssa.Label)
        ; instrs = ctx.instrs
        ; tail }
    ; before_return
    ; return } in
  { Ssa.procs = !(ctx.procs)
  ; main = main_proc }
