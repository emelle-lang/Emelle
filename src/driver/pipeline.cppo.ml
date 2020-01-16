(* Copyright (C) 2018-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

type t =
  { packages : (Qual_id.Prefix.t, Package.t) Hashtbl.t
  ; package : Package.t }

let create name packages =
  let package = Package.create name in
  let _ = Hashtbl.add packages ~key:name ~data:package in
  { package
  ; packages }

let compile_frontend self env ast_file =
  let open Result.Monad_infix in
  let typechecker = Typecheck.create self.package self.packages in
  Desugar.desugar typechecker env self.package self.packages ast_file
  >>= fun term_file ->
  Typecheck.typecheck typechecker term_file
  >>= fun typed_file ->
  Lower.compile self.package typed_file

let compile packages name ast_package =
  let open Result.Monad_infix in
  let st = create name packages in
  compile_frontend st (Env.empty (module String)) ast_package
  >>= Ssa_of_anf.compile_file
  >>= fun package ->
  Liveness.handle_file package
  >>= fun package' ->
  Color.handle_file package'
  >>= fun colorings ->
  To_asm.compile colorings package'

let compile_ret_ssa packages name ast_package =
  let open Result.Monad_infix in
  let st = create name packages in
  compile_frontend st (Env.empty (module String)) ast_package
  >>= Ssa_of_anf.compile_file
  >>= fun ssa_package ->
  Liveness.handle_file ssa_package
  >>= fun package ->
  Color.handle_file package
  >>= fun colorings ->
  To_asm.compile colorings package
  >>| fun asm_package ->
  (ssa_package, asm_package)

let compile_source_ssa packages prefix lexbuf =
  let open Result.Monad_infix in
  try Syntax_error.parse_file lexbuf >>= compile_ret_ssa packages prefix with
  | Lexer.Unclosed_comment loc ->
     Error (Message.Lexer_error(loc, Message.Unclosed_comment))
  | Lexer.Unclosed_string loc ->
     Error (Message.Lexer_error(loc, Message.Unclosed_string))

let create_hashtbl () =
  Hashtbl.create (module Qual_id.Prefix)

let compile_source packages prefix lexbuf =
  let open Result.Monad_infix in
  try Syntax_error.parse_file lexbuf >>= compile packages prefix with
  | Lexer.Unclosed_comment loc ->
     Error (Message.Lexer_error(loc, Message.Unclosed_comment))
  | Lexer.Unclosed_string loc ->
     Error (Message.Lexer_error(loc, Message.Unclosed_string))

let std_path str =
  { Qual_id.Prefix.package = "std"
  ; path = [str] }

let make_module packages prefix vm source_code =
  let module_object =
    match compile_source packages prefix (Lexing.from_string source_code) with
    | Ok compiled ->
       Eval.eval vm compiled
    | Error e ->
       let pp = Prettyprint.create () in
       Prettyprint.print_message Prettyprint.print_span pp e;
       Caml.print_endline (Prettyprint.to_string pp);
       assert false
  in
  Hashtbl.add_exn vm.Eval.eval'd_packages ~key:prefix ~data:module_object

let create_std packages vm =
  make_module packages (std_path "IO") vm {|
#include "../../std/io.ml"
  |};
  make_module packages (std_path "List") vm {|
#include "../../std/list.ml"
  |};
  make_module packages (std_path "Option") vm {|
#include "../../std/option.ml"
  |};
  make_module packages (std_path "Prelude") vm {|
export (id, const, puts)

let id = fun x -> x

let const = fun x _ -> x

let puts = foreign "puts" forall . String -> Unit
  |}
