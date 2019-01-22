open Base
open Js_of_ocaml
open Emelle

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
        let button = Dom_html.getElementById "compile" in
        let set_console_text str =
          console##.textContent := Js.some (Js.string str)
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
                 Prettyprint.Asm.print_module pp asm_module;
                 set_console_text (Prettyprint.to_string pp)
              | Error errs ->
                 Caml.print_endline "ERROR!";
                 Sequence.iter ~f:(Prettyprint.print_error pp) errs;
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
