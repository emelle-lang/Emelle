(* Copyright (C) 2019-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

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
    | Lit of Literal.t
    | Register of Register.t
end

(** The tag of a function box, whose first field is the function *)
val function_tag : int
