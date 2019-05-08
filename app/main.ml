(* Copyright (C) 2018-2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base
open Stdio
open Emmeline

let () =
  let open Result.Monad_infix in
  match
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.expr_eof Lexer.expr lexbuf in
    let package = Package.create "" in
    let env = Env.empty (module String) in
    let packages = Hashtbl.create (module String) in
    let desugarer = Desugar.create package packages in
    let typechecker = Typecheck.create package packages in
    Desugar.term_of_expr desugarer env ast
    >>= fun term ->
    Typecheck.infer_term typechecker term
    >>| fun typedtree ->
    Typecheck.gen typechecker typedtree.Typedtree.ty;
    typedtree.Typedtree.ty
  with
  | Ok ty ->
     let pprinter = Prettyprint.create () in
     Prettyprint.print_type pprinter (-1) ty;
     print_endline (Prettyprint.to_string pprinter)
  | Error _ -> ()
