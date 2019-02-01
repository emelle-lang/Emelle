open Base

type address = int

type operand =
  | Extern_var of Path.t
  | Lit of Literal.t
  | Stack of address

type instr =
  | Assign of address * operand * operand
  | Box of address * int * operand list
  | Box_dummy of address * int
  | Call of address * operand * operand * operand list
  | Deref of address * operand
  | Get of address * operand * int
  | Move of address * operand
  | Prim of address * string
  | Ref of address * operand
  | Set_field of operand * int * operand
  | Set_tag of operand * int
  | Tag of address * operand
  | Break of Ir.Label.t
  | Fail
  | Return of operand
  | Switch of operand * (int * Ir.Label.t) list * Ir.Label.t

type block = {
    block_params : address list;
    instrs : instr Queue.t;
  }

type proc = {
    free_vars : address list;
    params : address list;
    entry : Ir.Label.t;
    blocks : (Ir.Label.t, block, Ir.Label.comparator_witness) Map.t;
    frame_size : int;
  }

type file = {
    procs : (int, proc, Int.comparator_witness) Map.t;
    main : proc;
  }
