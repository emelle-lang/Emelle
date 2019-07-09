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

let std_path str =
  { Qual_id.Prefix.package = "std"
  ; path = [str] }

let create_vm rt =
  let ctx = Eval.create io rt in
  Graphics.add ctx;
  ctx

let create_std () =
  let std = Hashtbl.create (module Qual_id.Prefix) in
  let rt = Hashtbl.create (module Qual_id.Prefix) in

  let make_module mod_name source_code =
    let prefix = std_path mod_name in
    let module_interface, module_object =
      match
        Parser.file Lexer.expr (Lexing.from_string source_code)
        |> Pipeline.compile (Hashtbl.create (module Qual_id.Prefix)) prefix
      with
      | Ok(package, compiled) ->
         package, Eval.eval (create_vm rt) compiled
      | Error e ->
         let pp = Prettyprint.create () in
         Prettyprint.print_message Prettyprint.print_span pp e;
         Caml.print_endline (Prettyprint.to_string pp);
         assert false
      | exception e ->
         Caml.print_endline ("Exception when compiling module " ^ mod_name);
         raise e
    in
    Hashtbl.add_exn std ~key:prefix ~data:module_interface;
    Hashtbl.add_exn rt ~key:prefix ~data:module_object
  in
  make_module "IO" {|
#include "../std/io.ml"
  |};
  make_module "Option" {|
#include "../std/option.ml"
  |};
  make_module "Graphics" {|
#include "../std/graphics.ml"
  |};
  std, rt

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
           let std, rt = create_std () in
           match
             let prefix = { Qual_id.Prefix.package = ""; path = [] } in
             Pipeline.compile std prefix modl
           with
           | Ok (_, code) ->
              let ctx = create_vm rt in
              ignore (Eval.eval ctx code)
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
