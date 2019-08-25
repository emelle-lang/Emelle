open Base
open Js_of_ocaml
open Emmeline
open Grammar
open Compile

let typecheck_button =
  match
    Dom_html.getElementById "run"
    |> Dom_html.CoerceTo.button |> Js.Opt.to_option
  with
  | None -> assert false
  | Some button -> button

let console = Dom_html.getElementById "console"

let set_console_text str =
  console##.textContent := Js.some (Js.string str)

let append_console_text str =
  let old_str =
    match Js.Opt.to_option (console##.textContent) with
    | None -> ""
    | Some js_str -> Js.to_string js_str
  in set_console_text (old_str ^ str)

let io =
  { Io.putc = (fun _ -> ())
  ; puts = append_console_text }

let create_vm rt =
  let vm = Eval.create rt in
  Io.init io vm;
  Graphics.add vm;
  vm

let error_message =
  "There are errors in the program! Click the highlighted " ^
  "blocks to see the error message."

let () =
  typecheck_button##.onclick :=
    Dom.handler (fun _ ->
        set_console_text "";
        begin match compile_module entry_hole with
        | Error (Bexp.Hole hole, error) ->
           Bexp.Hole.set_error hole (fun () -> set_console_text error);
           set_console_text error_message
        | Ok modl ->
           let rt = Pipeline.create_hashtbl () in
           let vm = create_vm rt in
           let packages = Pipeline.create_hashtbl () in
           Pipeline.create_std packages vm;
           match
             let prefix = { Qual_id.Prefix.package = ""; path = [] } in
             Pipeline.compile packages prefix modl
           with
           | Ok code ->
              ignore (Eval.eval vm code)
           | Error e ->
              let rec f = function
                | Message.And(fst, snd) ->
                   f fst;
                   f snd
                | Message.Diagnostic d ->
                   let Bexp.Hole hole = d.Message.loc in
                   Bexp.Hole.set_error hole
                     (fun () ->
                       let pp = Prettyprint.create () in
                       Prettyprint.print_error pp d.Message.error;
                       set_console_text (Prettyprint.to_string pp))
                | Message.Unreachable str ->
                   Caml.print_endline ("Unreachable " ^ str);
              in
              f e;
              set_console_text error_message
        end;
        Js._false
      )

let () =
  Bexp.Toolbox.set_palette ctx.Bexp.toolbox modul_palette;
  Bexp.Workspace.render ctx
