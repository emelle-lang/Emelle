module Label : sig
  type t
  [@@deriving compare, hash, sexp]

  type gen

  type comparator_witness

  val comparator : (t, comparator_witness) Base.Comparator.comparator

  val create_gen : unit -> gen

  val fresh : gen -> t

  val to_string : t -> string
end

module Register : sig
  type t
  [@@deriving compare, hash, sexp]

  type gen

  type comparator_witness

  val comparator : (t, comparator_witness) Base.Comparator.comparator

  val create_gen : unit -> gen

  val fresh : gen -> t

  (** [gen_regs gen acc_init count] appends a list of [count] fresh registers
      onto [acc_init] using [gen] *)
  val gen_regs : gen -> t list -> int -> t list

  val to_string : t -> string
end

module Operand : sig
  type t =
    | Extern_var of Path.t
    | Lit of Literal.t
    | Register of Register.t
end

(** The tag of a function box, whose first field is the function *)
val function_tag : int