open Base

type address = int

type operand =
  | Extern_var of Path.t
  | Lit of Literal.t
  | Stack of address

type instr =
  | Assign of address * operand * operand
  | Box of address * operand list
  | Box_dummy of address * int
  | Call of address * operand * operand * operand list
  | Deref of address * operand
  | Fun of address * int * operand list
  | Get of address * operand * int
  | Move of address * operand
  | Memcopy of address * operand * operand
  | Prim of address * string
  | Ref of address * operand
  | Break of Ssa.Label.t
  | Fail
  | Return of operand
  | Switch of operand * (int * Ssa.Label.t) list * Ssa.Label.t

type block = {
    instrs : instr Queue.t;
    phis : (address * int) Queue.t (* dest color, index *)
  }

type proc = {
    free_vars : int list;
    params : int list;
    blocks : (Ssa.Label.t, block, Ssa.Label.comparator_witness) Map.t;
  }

type package = {
    procs : (int, proc, Int.comparator_witness) Map.t;
    main : proc;
  }
