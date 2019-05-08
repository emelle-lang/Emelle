(* Copyright (C) 2018-2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base
open Js_of_ocaml
open Emmeline

let () =
  Dom_html.window##.onload :=
    Dom.handler (fun _ ->
        let textarea =
          match
            Dom_html.getElementById_coerce "editor" Dom_html.CoerceTo.textarea
          with
          | None -> assert false
          | Some textarea -> textarea
        in
        let console = Dom_html.getElementById "console" in
        let button = Dom_html.getElementById "run" in
        let set_console_text str =
          console##.textContent := Js.some (Js.string str)
        in
        let append_console_text str =
          let old_str =
            match Js.Opt.to_option (console##.textContent) with
            | None -> ""
            | Some js_str -> Js.to_string js_str
          in console##.textContent := Js.some (Js.string (old_str ^ str))
        in
        let io =
          { Io.putc = (fun _ -> ())
          ; puts = append_console_text }
        in
        button##.onclick :=
          Dom.handler (fun _ ->
              let pp = Prettyprint.create () in
              begin match
                let program = textarea##.value in
                let bytestr = Js.to_bytestring program in
                Parser.file Lexer.expr (Lexing.from_string bytestr)
                |> Pipeline.compile (Hashtbl.create (module String)) "main"
              with
              | Ok (_, asm_module) ->
                 Caml.print_endline "OK!";
                 let ctx = Eval.create io in
                 ignore (Eval.eval ctx asm_module)
              | Error errs ->
                 Caml.print_endline "ERROR!";
                 Prettyprint.print_message Prettyprint.print_span pp errs;
                 set_console_text (Prettyprint.to_string pp)
              | exception (Parser.Error | Lexer.Error _) ->
                 Caml.print_endline "ERROR";
                 set_console_text "Syntax error"
              | exception _ ->
                 Caml.print_endline "ERROR";
                 set_console_text "Unknown error"
              end;
              Js._true
            );
        Caml.print_endline "loaded";
        Js._true
      )
