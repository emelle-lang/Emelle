open Base

type t = {
    putc : char -> unit;
    puts : string -> unit;
  }

let stdio =
  { putc = Caml.print_char
  ; puts = Caml.print_string }
