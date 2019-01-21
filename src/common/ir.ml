open Base

module Label = struct
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

  let to_string = Int.to_string
end

module Register = struct
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

  let rec gen_regs list = function
    | 0 -> list
    | n -> gen_regs ((n - 1)::list) (n - 1)

  let to_string = Int.to_string
end

module Operand = struct
  type t =
    | Extern_var of Path.t
    | Lit of Literal.t
    | Register of Register.t
end
