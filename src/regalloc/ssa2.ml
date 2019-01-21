open Base

type instr = {
    dest : int;
    opcode : Ssa.opcode;
    ending_regs : (int, Int.comparator_witness) Set.t;
  }

type basic_block = {
    preds : (Ssa.Label.t, Ssa.Label.comparator_witness) Set.t;
    instrs : instr list;
    jump : Ssa.jump;
    ending_at_jump : (int, Int.comparator_witness) Set.t;
  }

type proc = {
    free_vars : Anf.register list;
    params : Anf.register list;
    entry : Ssa.Label.t;
    blocks : (int, basic_block, Int.comparator_witness) Map.t;
    before_return : Ssa.Label.t;
  }

type package = {
    procs : (int, proc, Int.comparator_witness) Map.t;
    main : proc
  }
