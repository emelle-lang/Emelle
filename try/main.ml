(* Copyright (C) 2018-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base
open Js_of_ocaml
open Emmeline

let main_prefix = { Qual_id.Prefix.package = "Repl"; path = [] }

let hello_world_program = {|
(* Primitive function *)
let puts = foreign "puts" forall . String -> Unit

let () = puts "Hello, world!\n"
|}

let list_program = {|
(* Emmeline supports recursive ADTs *)
type List a = Nil | Cons a * (List a)

let rec iter = fun
  | _ Nil -> ()
  | f (Cons x xs) ->
     f x;
     iter f xs

let puts = foreign "puts" forall . String -> Unit

let () = iter puts (Cons "1\n" (Cons "2\n" (Cons "3\n" Nil)))
|}

let records_program = {|
(* Emmeline features records with
   polymorphic fields *)

(* Endofunctor *)
type Functor f = {
  map : forall a b. (a -> b) -> f a -> f b
}

type Product a b = Pair a * b

type Applicative f = {
  functor : forall. Functor f;
  unit : forall. f Unit;
  product : forall a b. f a -> f b -> f (Product a b)
}

let map2 = fun appl f a b ->
  appl.functor.map (fun (Pair a b) -> f a b) (appl.product a b)

type Option a = None | Some a

let opt_functor = {
  map = (fun
    | _ None -> None
    | f (Some x) -> Some (f x)
  )
}

let opt_applicative = {
  functor = opt_functor;
  unit = Some ();
  product = (fun
    | (Some a) (Some b) -> Some (Pair a b)
    | _ _ -> None
  )
}

let puts = foreign "puts" forall . String -> Unit

let _ = opt_functor.map puts (Some "Hello world!\n")

let puts2 = fun x y ->
  puts x;
  puts y

let _ = map2 opt_applicative puts2 (Some "Hello ") (Some "world!")
|}

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
        let menu =
          match
            Dom_html.getElementById_coerce "example-select"
              Dom_html.CoerceTo.select
          with
          | None -> assert false
          | Some menu -> menu
        in
        let console = Dom_html.getElementById "console" in
        let button = Dom_html.getElementById "run" in
        let set_textarea_text str =
          textarea##.value := Js.string str
        in
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
                Lexing.from_string bytestr
                |> Pipeline.compile_source
                     (Hashtbl.create (module Qual_id.Prefix)) main_prefix
              with
              | Ok asm_module ->
                 Caml.print_endline "OK!";
                 set_console_text "";
                 let vm =
                   Eval.create (Hashtbl.create (module Qual_id.Prefix))
                 in
                 Io.init io vm;
                 ignore (Eval.eval vm asm_module)
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
        menu##.onchange :=
          Dom.handler (fun _ ->
              begin match menu##.selectedIndex with
              | 0 -> set_textarea_text hello_world_program
              | 1 -> set_textarea_text list_program
              | 2 -> set_textarea_text records_program
              | _ -> assert false
              end;
              Js._true
            );
        set_textarea_text hello_world_program;
        Caml.print_endline "loaded";
        Js._true
      )
