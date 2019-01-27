open Base

type t =
  | Char of char
  | Float of float
  | Int of int
  | String of string
  | Unit
[@@deriving compare, hash, sexp]
