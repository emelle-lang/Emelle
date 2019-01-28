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

  let rec gen_regs list = function
    | 0 -> list
    | n -> gen_regs ((n - 1)::list) (n - 1)

  let to_string r = "r"^(Int.to_string r)
end

module Operand = struct
  type t =
    | Extern_var of Path.t
    | Lit of Literal.t
    | Register of Register.t
end

let function_tag = 250
