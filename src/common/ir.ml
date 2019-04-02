(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base

module IntId = struct
  module T = struct
    type t = int
    [@@deriving compare, hash, sexp]
  end
  include T
  include Comparator.Make(T)

  type gen = t ref

  let create_gen () = ref 0

  let fresh gen =
    let id = !gen in
    gen := id + 1;
    id
end

module Label = struct
  include IntId

  let to_string l = "L"^(Int.to_string l)
end

module Register = struct
  include IntId

  let gen_regs gen init count =
    let offset = !gen in
    gen := offset + count;
    let rec go acc = function
      | 0 -> acc
      | n -> go ((offset + n - 1)::acc) (n - 1)
    in go init count

  let to_string r = "r"^(Int.to_string r)
end

module Operand = struct
  type t =
    | Extern_var of Path.t
    | Lit of Literal.t
    | Register of Register.t
end

let function_tag = 250
