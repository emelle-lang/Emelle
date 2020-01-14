(* Copyright (C) 2018-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base
open Stdio
open Emmeline

let main_prefix = { Qual_id.Prefix.package = ""; path = [] }

let () =
  match
    let lexbuf = Lexing.from_channel stdin in
    Parser.file Lexer.expr lexbuf
    |> Pipeline.compile (Hashtbl.create (module Qual_id.Prefix)) main_prefix
  with
  | Ok asm_module ->
     let vm = Eval.create (Hashtbl.create (module Qual_id.Prefix)) in
     Io.init Io.stdio vm;
     ignore (Eval.eval vm asm_module)
  | Error e ->
     let pp = Prettyprint.create () in
     Prettyprint.print_message Prettyprint.print_span pp e;
     Stdio.print_endline (Prettyprint.to_string pp)
