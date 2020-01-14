(* Copyright (C) 2018-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

(** This transformation turns ANF join points, which are local to the case expr,
    into SSA basic blocks, which are local to the function. Here, the compiler
    compiles decision trees into switches, jumps, and basic block parameters. *)

open Base

type jump_dest =
  | Label of Ir.Label.t
  | Return

type t = {
    procs : (int, Ssa.proc, Int.comparator_witness) Map.t ref;
    blocks :
      (Ir.Label.t, Ssa.basic_block, Ir.Label.comparator_witness) Map.t ref;
    label_gen : Ir.Label.gen;
    proc_gen : int ref;
    reg_gen : Ir.Register.gen;
    instrs : Ssa.instr Queue.t;
    curr_block : Ir.Label.t;
    dest : Ir.Register.t;
    jump_dest : jump_dest;
  }

(** Helper record for organizational purposes *)
type branch = {
    branch_idx : Ir.Label.t;
    labels : Ir.Label.t list;
    block : Ssa.basic_block;
  }

(** [fresh_block ctx ~cont] applies [cont] to the index of the next basic block,
    [cont] returns a tuple consisting of a result, the block predecessors, the
    instruction queue, and the block successor. The entire function returns
    [cont]'s result *)
let fresh_block ctx ~cont =
  let open Result.Let_syntax in
  let idx = Ir.Label.fresh ctx.label_gen in
  let%map ret, params, preds, instrs, jump = cont idx in
  let block = { Ssa.instrs; params; preds; jump } in
  ctx.blocks := Map.set !(ctx.blocks) ~key:idx ~data:block;
  (ret, block)

(** [compile_decision_tree ctx instrs this_label branches tree]

    - ctx: The value that holds the state
    - instrs: A queue of instructions for the current basic block being
        generated
    - this_label: The label of the current basic block being generated
    - branches: An array of branches to jump to at the leaf nodes
    - tree: The decision tree to compile *)
let rec compile_decision_tree
          (ctx : t)
          (instrs : Ssa.instr Queue.t)
          (this_label : Ir.Label.t)
          (branches : branch array) =
  let open Result.Let_syntax in
  function
  | Anf.Deref(occ, dest, tree) ->
     Queue.enqueue instrs (Ssa.Deref(dest, occ));
     compile_decision_tree ctx instrs this_label branches tree
  | Anf.Fail -> Ok Ssa.Fail
  | Anf.Leaf(operands, idx) ->
     let { branch_idx; block; _ } = branches.(idx) in
     block.Ssa.preds <- Set.add block.Ssa.preds this_label;
     Ok (Ssa.Break(branch_idx, operands))
  | Anf.Switch(tag_reg, occ, trees, else_tree) ->
     Queue.enqueue instrs (Tag(tag_reg, occ));
     let%bind cases =
       Map.fold trees ~init:(Ok []) ~f: begin
           fun ~key:case ~data:(regs, tree) acc ->
           let%bind list = acc in
           let%map (result, _) =
             fresh_block ctx ~cont:begin fun case_idx ->
               let case_instrs = Queue.create () in
               let%map jump =
                 List.iteri ~f:(fun idx reg ->
                     Queue.enqueue case_instrs (Ssa.Get(reg, occ, idx))
                   ) regs;
                 compile_decision_tree ctx case_instrs case_idx branches tree
               in
               ( (case, case_idx) :: list (* result *)
               , []
               , Set.singleton (module Ir.Label) this_label (* predecessors *)
               , case_instrs
               , jump )
               end
           in result
         end in
     let%map (else_block_idx, _) =
       fresh_block ctx ~cont:(fun else_idx ->
           let else_instrs = Queue.create () in
           let%map jump =
             compile_decision_tree ctx else_instrs else_idx branches else_tree
           in
           ( else_idx
           , []
           , Set.singleton (module Ir.Label) this_label (* predecessors *)
           , else_instrs
           , jump )
         )
     in Ssa.Switch(Ir.Operand.Register tag_reg, cases, else_block_idx)

let rec compile_join_points ctx join_points dest cont_jump_dest =
  let open Result.Let_syntax in
  List.fold_right join_points ~init:(Ok [])
    ~f:(fun (reg_args, instr) acc ->
      let%bind list = acc in
      (* The basic block for the join point *)
      let%map (branch_idx, labels), block =
        fresh_block ctx ~cont:(fun branch_idx ->
            let branch_instrs = Queue.create () in
            let%map labels, jump =
              compile_instr
                { ctx with
                  instrs = branch_instrs
                ; curr_block = branch_idx
                ; dest
                ; jump_dest = cont_jump_dest }
                instr in
            ( (branch_idx, labels)
            , reg_args
            , Set.empty (module Ir.Label)
            , branch_instrs
            , jump )
          )
      in { branch_idx; labels; block } :: list
    )

and compile_opcode ctx dest anf ~cont
        : (Ir.Label.t list * Ssa.jump, 'a Message.t) Result.t =
  let open Result.Let_syntax in
  match anf with
  | Anf.Assign(lval, rval) ->
     Queue.enqueue ctx.instrs (Ssa.Assign(dest, lval, rval));
     cont ctx
  | Anf.Box(tag, contents) ->
     Queue.enqueue ctx.instrs (Ssa.Box(dest, tag, contents));
     cont ctx
  | Anf.Call(f, arg, args) ->
     Queue.enqueue ctx.instrs (Ssa.Call(dest, f, arg, args));
     cont ctx
  | Anf.Case(tree, join_points) ->
     let%map result, _ =
       (* The basic block for the case expr confluent basic block *)
       fresh_block ctx ~cont:(fun confl_idx ->
           let confl_instrs = Queue.create () in
           let%bind branches =
             compile_join_points ctx join_points dest (Label confl_idx) in
           let%bind jump_from_decision_tree =
             compile_decision_tree ctx ctx.instrs ctx.curr_block
               (Array.of_list branches) tree in
           let preds =
             List.fold branches ~init:(Set.empty (module Ir.Label))
               ~f:(fun acc { labels; _ } ->
                 List.fold labels ~init:acc ~f:Set.add
               ) in
           (* Label of confluent block, jump of confluent block *)
           let%map label, jump =
             (* Compile the rest of the ANF in the confluent block *)
             cont { ctx with instrs = confl_instrs; curr_block = confl_idx } in
           ( (label, jump_from_decision_tree)
           , []
           , preds
           , confl_instrs
           , jump )
         ) in result
  | Anf.Fun proc ->
     let env = List.map ~f:(fun (_, op) -> op) proc.Anf.env in
     let%bind proc = compile_proc ctx proc in
     let idx = !(ctx.proc_gen) in
     ctx.proc_gen := idx + 1;
     let procs = Map.set !(ctx.procs) ~key:idx ~data:proc in
     ctx.procs := procs;
     Queue.enqueue ctx.instrs
       (Ssa.Box
          ( dest
          , Ir.function_tag
          , (Ir.Operand.Lit (Literal.Int idx))::env ));
     cont ctx
  | Anf.Get(operand, offset) ->
     Queue.enqueue ctx.instrs (Ssa.Get(dest, operand, offset));
     cont ctx
  | Anf.Load o ->
     Queue.enqueue ctx.instrs (Ssa.Load(dest, o));
     cont ctx
  | Anf.Package str ->
     Queue.enqueue ctx.instrs (Ssa.Package(dest, str));
     cont ctx
  | Anf.Prim p ->
     Queue.enqueue ctx.instrs (Ssa.Prim(dest, p));
     cont ctx
  | Anf.Ref x ->
     Queue.enqueue ctx.instrs (Ssa.Ref(dest, x));
     cont ctx

and compile_instr ctx anf
    : (Ir.Label.t list * Ssa.jump, 'a Message.t) Result.t =
  let open Result.Let_syntax in
  match anf.Anf.instr with
  | Anf.Break(Anf.Call(f, arg, args)) when
         (match ctx.jump_dest with
          | Return -> true
          | Label _ -> false) ->
     Ok ([], Ssa.Tail_call(f, arg, args))
  | Anf.Break(Anf.Case(tree, join_points)) ->
     let%bind branches =
       compile_join_points ctx join_points ctx.dest ctx.jump_dest in
     let%map jump =
       compile_decision_tree ctx ctx.instrs ctx.curr_block
         (Array.of_list branches) tree in
     let labels =
       List.fold branches ~init:[] ~f:(fun acc { labels; _ } ->
           List.append labels acc
         )
     in (labels, jump)
  | Anf.Break op ->
     compile_opcode ctx ctx.dest op ~cont:(fun ctx ->
         Ok ( [ctx.curr_block]
            , match ctx.jump_dest with
              | Label label -> Ssa.Break(label, [])
              | Return -> Ssa.Return )
       )
  | Anf.Let(reg, op, next) ->
     compile_opcode ctx reg op ~cont:(fun ctx ->
         compile_instr ctx next
       )
  | Anf.Let_rec(bindings, next) ->
     (* Initialize registers with dummy allocations *)
     List.iter bindings ~f:(fun (reg, def) ->
         let size =
           match def with
           | Anf.Rec_box(_, list) -> List.length list
           | Anf.Rec_fun proc -> List.length proc.Anf.env + 1
         in
         Queue.enqueue ctx.instrs (Ssa.Box_dummy(reg, size))
       );
     let%bind () =
       List.fold_result bindings ~init:() ~f:(fun () (reg, op) ->
           let dest = Ir.Operand.Register reg in
           match op with
           | Anf.Rec_box(tag, list) ->
              List.iteri list ~f:(fun i operand ->
                  Queue.enqueue ctx.instrs (Ssa.Set_field(dest, i, operand))
                );
              Ok (Queue.enqueue ctx.instrs (Ssa.Set_tag(dest, tag)))
           | Anf.Rec_fun proc ->
              let%map proc' = compile_proc ctx proc in
              let idx = !(ctx.proc_gen) in
              ctx.proc_gen := idx + 1;
              let procs = Map.set !(ctx.procs) ~key:idx ~data:proc' in
              ctx.procs := procs;
              Queue.enqueue ctx.instrs
                (Ssa.Set_field(dest, 0, Ir.Operand.Lit (Literal.Int idx)));
              List.iteri proc.Anf.env ~f:(fun i (_, operand) ->
                  Queue.enqueue ctx.instrs (Ssa.Set_field(dest, i + 1, operand))
                );
              Queue.enqueue ctx.instrs (Ssa.Set_tag(dest, Ir.function_tag))
         ) in
     compile_instr ctx next

and compile_proc ctx proc =
  let open Result.Let_syntax in
  let blocks = ref (Map.empty (module Ir.Label)) in
  let instrs = Queue.create () in
  let label_gen = Ir.Label.create_gen () in
  let entry_label = Ir.Label.fresh label_gen in
  let state =
    { procs = ctx.procs
    ; proc_gen = ctx.proc_gen
    ; blocks
    ; instrs
    ; label_gen
    ; reg_gen = proc.Anf.reg_gen
    ; curr_block = entry_label
    ; dest = proc.Anf.return
    ; jump_dest = Return } in
  let%map before_returns, jump = compile_instr state proc.Anf.body in
  let entry_block =
    { Ssa.preds = Set.empty (module Ir.Label)
    ; params = []
    ; instrs
    ; jump } in
  { Ssa.free_vars = List.map ~f:(fun (reg, _) -> reg) proc.Anf.env
  ; params = proc.Anf.params
  ; blocks = Map.set !blocks ~key:entry_label ~data:entry_block
  ; entry = entry_label
  ; before_returns
  ; return = proc.Anf.return }

let compile_file anf =
  let open Result.Let_syntax in
  let label_gen = Ir.Label.create_gen () in
  let entry_label = Ir.Label.fresh label_gen in
  let ctx =
    { procs = ref (Map.empty (module Int))
    ; blocks = ref (Map.empty (module Ir.Label))
    ; instrs = Queue.create ()
    ; proc_gen = ref 0
    ; label_gen
    ; reg_gen = anf.Anf.reg_gen
    ; curr_block = entry_label
    ; jump_dest = Return
    ; dest = anf.Anf.top_return } in
  let%map before_returns, jump = compile_instr ctx anf.Anf.top_instr in
  let entry_block =
    { Ssa.preds = Set.empty (module Ir.Label)
    ; params = []
    ; instrs = ctx.instrs
    ; jump } in
  let main_proc =
    { Ssa.free_vars = []
    ; params = []
    ; blocks = Map.set !(ctx.blocks) ~key:entry_label ~data:entry_block
    ; entry = entry_label
    ; before_returns
    ; return = anf.Anf.top_return } in
  { Ssa.procs = !(ctx.procs)
  ; main = main_proc }
