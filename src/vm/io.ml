(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

type t = {
    putc : char -> unit;
    puts : string -> unit;
  }

let add_putc t vm =
  Eval.add_foreign_fun vm "putc"
    (Eval.foreign ~arity:1 (fun _ args ->
         match args with
         | [|`Char c|] ->
            t.putc c;
            `Unit
         | _ -> failwith "putc: Type error"))

let add_puts t vm =
  Eval.add_foreign_fun vm "puts"
    (Eval.foreign ~arity:1 (fun _ args ->
         match args with
         | [|`String s|] ->
            t.puts s;
            `Unit
         | _ -> failwith "Puts: Type error"))

let init t vm =
  add_putc t vm;
  add_puts t vm

let stdio =
  { putc = Caml.print_char
  ; puts = Caml.print_string }
