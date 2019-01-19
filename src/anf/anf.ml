open Base

type register = int

type operand =
  | Extern_var of Path.t
  | Lit of Literal.t
  | Register of int

(** A jump is a branch instruction to a join point with the given arguments *)
and jump = operand list * int

type decision_tree =
  | Deref of operand * register * decision_tree
  | Fail
  | Leaf of jump
    (** A leaf holds a mapping from idents to pattern match occurrences. *)
  | Switch of
      register * operand
      * (int, register list * decision_tree, Int.comparator_witness) Map.t
      * decision_tree
    (** A switch holds the scrutinee occurrence and a map from constructors to
        decision trees, and a default decision tree. *)

type 'a opcode =
  | Assign of operand * operand
  | Box of operand list
  | Call of operand * operand * operand list
    (** proc, first arg, rest args *)
  | Case of decision_tree * 'a join_point list
    (** decision tree, jump table *)
  | Fun of 'a proc
  | Load of operand
  | Prim of string
  | Ref of operand

(** A join point is like a basic block with parameters *)
and 'a join_point = register list * 'a instr

and 'a instr' =
  | Break of operand
  | Let of register * 'a opcode * 'a instr
  | Let_rec of (register * register * 'a opcode) list * 'a instr
  | Seq of 'a opcode * 'a instr

and 'a instr = {
    instr : 'a instr';
    ann : 'a;
  }

and 'a proc = {
    env : (register * operand) list; (** The captured variables *)
    params : register list;
    body : 'a instr;
  }
