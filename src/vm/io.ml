(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base

type t = {
    putc : char -> unit;
    puts : string -> unit;
  }

let stdio =
  { putc = Caml.print_char
  ; puts = Caml.print_string }
