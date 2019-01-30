open Base

type instr = {
    dest : Ir.Register.t option;
    opcode : Ssa.instr;
    ending_regs : (Ir.Register.t, Ir.Register.comparator_witness) Set.t;
  }

type basic_block = {
    params : Ir.Register.t list;
    preds : (Ir.Label.t, Ir.Label.comparator_witness) Set.t;
    instrs : instr list;
    jump : Ssa.jump;
    ending_at_jump : (Ir.Register.t, Ir.Register.comparator_witness) Set.t;
  }

type proc = {
    free_vars : Ir.Register.t list;
    params : Ir.Register.t list;
    entry : Ir.Label.t;
    blocks : (Ir.Label.t, basic_block, Ir.Label.comparator_witness) Map.t;
    before_return : Ir.Label.t;
  }

type file = {
    procs : (int, proc, Int.comparator_witness) Map.t;
    main : proc
  }
