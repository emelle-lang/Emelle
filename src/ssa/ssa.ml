open Base

module Label = struct
  module T = struct
    type t = int
    [@@deriving compare, hash, sexp]
  end
  include T
  include Comparator.Make(T)
end

type operand = Anf.operand

type jump =
  | Break of Label.t * operand list (** Break to a basic block *)
  | Fail (** Pattern match failure *)
  | Return of operand
  | Switch of operand * (int * Label.t) list * Label.t
      (** The jump is dynamic *)

type opcode =
  | Assign of operand * operand
  | Box of operand list
  | Box_dummy of int
  | Call of operand * operand * operand list
  | Deref of operand
  | Fun of int * operand list
  | Get of operand * int
  | Load of operand
  | Memcopy of operand * operand
  | Phi of int
  | Prim of string
  | Ref of operand

type instr = {
    dest : int option;
    opcode : opcode;
  }

type basic_block = {
    mutable preds : (Label.t, Label.comparator_witness) Set.t;
    instrs : instr Queue.t;
    jump : jump;
  }

type proc = {
    free_vars : Anf.register list;
    params : Anf.register list;
    entry : Label.t;
    blocks : (Label.t, basic_block, Label.comparator_witness) Map.t;
    before_return : Label.t;
  }

type package = {
    procs : (int, proc, Int.comparator_witness) Map.t;
    main : proc
  }

let successors = function
  | Break(label, _) -> [label]
  | Return _ -> []
  | Fail -> []
  | Switch(_, cases, else_case) ->
     else_case::(List.map ~f:(fun (_, label) -> label) cases)
