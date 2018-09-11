open Base
open Emelle

type stage =
  | Syntax
  | Desugar
  | Typecheck
  | End (* Used when the test should pass all the stages *)
[@@deriving compare]

exception Fail of string * stage

let make_test (a, b) = (a, a, b)

let test f stage =
  List.fold_right ~f:(
      fun (repr, text, fail_stage) acc ->
      match f repr, compare_stage stage fail_stage with
      (* Succeeded, supposed to succeed *)
      | Ok next, _ ->
         (next, text, fail_stage)::acc
      (* Failed, supposed to fail *)
      | Error _, 0 ->
         acc
      | _ ->
         raise (Fail(text, stage))
    ) ~init:[]

let optionally f x =
  try Ok (f x) with
  | Lexer.Error str -> Error (Sequence.return (Message.Lexer_error str))
  | Parser.Error -> Error (Sequence.return Message.Parser_error)

let tests =
  List.map ~f:make_test
    [ "fun", Syntax
    ; "case", Syntax
    ; "with", Syntax
    ; "fun (some x) -> x", Syntax
    ; "fun -> x", Syntax
    ; "f", Desugar
    ; "f x", Desugar
    ; "fun x -> y", Desugar
    ; "case x with | y -> y", Desugar
    ; "let f = fun x -> f x in f", Desugar
    ; "let f = fun x -> x and g = f in g", Desugar
    ; "let g = f and f = fun x -> x in g", Desugar
    ; "case fun x -> x with | x -> x | y -> x", Desugar
    ; "fun x -> x x", Typecheck
    ; "fun f -> let g = fun x -> f (x x) in g g", Typecheck
    ; "let rec g = f and f = fun x -> x in g", End
    ; "let rec f = fun x -> x and g = f in g", End
    ; "let rec f = fun x -> f x in f", End
    ; "case fun x -> x with | x -> x | y -> y", End
    ; "fun x -> x", End
    ; "fun x -> fun y -> x", End
    ; "fun x y -> x", End
    ; "fun f x -> f x", End
    ; "fun f x y -> f y x", End
    ]

let asts =
  test (
      optionally (fun str -> Parser.file Lexer.expr (Lexing.from_string str))
    ) Syntax tests

let desugar expr =
  let env = Env.empty (module String) in
  let desugarer = Desugar.create () in
  Desugar.term_of_expr desugarer env expr

let terms =
  test desugar Desugar asts

let typecheck term =
  let open Result.Monad_infix in
  let typechecker = Typecheck.create () in
  Typecheck.infer typechecker term >>| fun _ ->
  term

let terms = test typecheck Typecheck terms
