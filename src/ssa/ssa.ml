open Base

type operand = Anf.operand

type jump =
  | Break of Ir.Label.t * operand list (** Break to a basic block *)
  | Fail (** Pattern match failure *)
  | Return of operand
  | Switch of operand * (int * Ir.Label.t) list * Ir.Label.t
      (** The jump is dynamic *)

type opcode =
  | Assign of operand * operand
  | Box of int * operand list
  | Box_dummy of int
  | Call of operand * operand * operand list
  | Deref of operand
  | Get of operand * int
  | Load of operand
  | Memcopy of operand * operand
  | Phi of int
  | Prim of string
  | Ref of operand
  | Tag of operand

type instr = {
    dest : Ir.Register.t;
    opcode : opcode;
  }

type basic_block = {
    mutable preds : (Ir.Label.t, Ir.Label.comparator_witness) Set.t;
    instrs : instr Queue.t;
    jump : jump;
  }

type proc = {
    free_vars : Anf.register list;
    params : Anf.register list;
    entry : Ir.Label.t;
    blocks : (Ir.Label.t, basic_block, Ir.Label.comparator_witness) Map.t;
    before_return : Ir.Label.t;
  }

type file = {
    procs : (int, proc, Int.comparator_witness) Map.t;
    main : proc
  }

let successors = function
  | Break(label, _) -> [label]
  | Return _ -> []
  | Fail -> []
  | Switch(_, cases, else_case) ->
     else_case::(List.map ~f:(fun (_, label) -> label) cases)
