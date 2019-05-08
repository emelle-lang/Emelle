open Base
open Emmeline

exception Fail of string

let test_valid parse =
  List.iter ~f:(
      fun program ->
      try let _ = parse Lexer.expr (Lexing.from_string program) in () with
      | _ -> raise (Fail program)
    )

let test_invalid parse =
  List.iter ~f:(
      fun program ->
      try
        let _ = parse Lexer.expr (Lexing.from_string program) in
        assert false
      with
      | Parser.Error -> ()
      | Lexer.Error _ -> ()
      | exn -> raise exn
    )

let valid_exprs =
  [ "f x"
  ; "let x = y in x"
  ; "fun x y -> y"
  ; "let Option.Some x = n in x"
  ; "fun (Option.Some x) -> x"
  ; "  y  x  cs w  uii8'9"
  ; "case x with | x -> x"
  ; "case x with x -> x"
  ; " case ( x  ) with x -> x | y -> z"
  ; "fun x -> x | y -> y"
  ; "  fun   |x -> x | y  -> y" ]

let invalid_exprs =
  [ "fun"
  ; "case"
  ; "fun (some x) -> x"
  ; "fun -> x" ]

let valid_monotypes =
  [ "a -> b"
  ; "(->) a"
  ; "(->)"
  ; "(->) a b"
  ; "a b c"
  ; "(a -> b) -> f a -> f b"
  ; "m a -> (a -> m b) -> m b"
  ; "Option a"
  ; "Either a b"
  ; "Either foo bar"
  ; "IO Unit"
  ; "Unit"
  ; "a' b' c'"
  ]

let invalid_monotypes =
  [ "->"
  ; "a ->"
  ; "123 a"
  ; "case"
  ; "with"
  ; "forall"
  ; "forall a . a"
  ; "fun x -> x"
  ]

let valid_adts =
  [ "Void ="
  ; "Void = |"
  ; "Option a = Some a | None"
  ; "List a = | Cons a * List a | Nil"
  ; "Bool = False | True"
  ; "Either e a = Left e | Right a"
  ; "StateT s m a = StateT s -> m (Product a s)"
  ]

let invalid_adts =
  [ ""
  ; "= "
  ; "X = Y |"
  ; "x = Y"
  ; "TupleSyntaxNotSupported a b = Foo (a, b)"
  ]

let () =
  test_valid Parser.expr_eof valid_exprs;
  test_invalid Parser.expr_eof invalid_exprs;
  test_valid Parser.monotype_eof valid_monotypes;
  test_invalid Parser.monotype_eof invalid_monotypes;
  test_valid Parser.adt_eof valid_adts;
  test_invalid Parser.adt_eof invalid_adts
