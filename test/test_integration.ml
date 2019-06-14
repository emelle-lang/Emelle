open Base
open Emmeline

type phase =
  | Syntax
  | Desugar
  | Typecheck
  | End (* Used when the test should pass all the stages *)
[@@deriving compare]

exception Fail of string * phase

let optionally f x =
  try Ok (f x) with
  | Lexer.Error str -> Error (Sequence.return (Message.Lexer_error str))
  | Parser.Error -> Error (Sequence.return Message.Parser_error)

let tests =
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
  ; "0 0", Typecheck
  ; "let rec g = f and f = fun x -> x in g", End
  ; "let rec f = fun x -> x and g = f in g", End
  ; "let rec f = fun x -> f x in f", End
  ; "case fun x -> x with | x -> x | y -> y", End
  ; "fun x -> x", End
  ; "fun x -> fun y -> x", End
  ; "fun x y -> x", End
  ; "fun f x -> f x", End
  ; "fun f x y -> f y x", End
  ; "let id = fun x -> x in id id", End
  ; "let rec id = fun x -> x and id2 = id in id id2 (id2 id)", End
  ; "case fun x -> x with id -> id id", End
  ; "let id = fun x -> x in case id with x -> x x", End
  ; "let two = fun _ -> 2 in two 5", End
  ; "let two = fun _ -> 2.0 in two 9", End
  ; "let two = fun _ -> +2 in two 6", End
  ; "let two = fun _ -> +2.0 in two 4", End
  ; "let minus_five = fun _ -> -5 in minus_five 0", End
  ; "(fun x -> x) 0", End
  ; "\"\\\\\"", End
  ; "\"\\\"\"", End
  ; "\"\\\'\"", End
  ; "\"foobar\\\"baz\"", End
  ; "\"Hello world!\\n\"", End
  ; "\"\"", End
  ; "let f = fun x y -> x; y in f", End
  ; "let assign = fun l r -> l := r in assign", End
  ; "Ref 0", End
  ; "(Ref 1) := 0", End
  ; "Ref", End ]

let test_phase f curr_phase input format phase =
  let result = f format in
  match result, (compare_phase phase curr_phase) = 0 with
  | Error _, true -> None
  | Error _, false -> raise (Fail(input, curr_phase))
  | Ok _, true -> raise (Fail(input, curr_phase))
  | Ok next, false -> Some next

let prefix = { Qual_id.Prefix.package = ""; path = [] }

let test (input, phase) =
  let open Option.Monad_infix in
  let next =
    test_phase (optionally (fun str ->
              Parser.expr_eof Lexer.expr (Lexing.from_string str)
      )) Syntax input input phase
  in
  next >>= fun next ->
  let package = Package.create prefix in
  let env = Env.empty (module String) in
  let packages = Hashtbl.create (module Qual_id.Prefix) in
  let desugarer = Desugar.create package packages in
  let typechecker = Typecheck.create package packages in
  test_phase
    (Desugar.term_of_expr desugarer typechecker env) Desugar input next phase
  >>= fun next ->
  test_phase (Typecheck.infer_term typechecker) Typecheck input next phase

let _ = List.map ~f:test tests

exception Module_fail of string

let tests =
  [ "export (id) let id = fun x -> x"
  ; "export (k) let k = fun x _ -> x"
  ; "export (id, k) let id = fun x -> x let k = fun x _ -> x"
  ; "export () let _ = let x = 1 in let y = x in let z = x in z"
  ; "type Option a = None | Some a"
  ; "export (id2, id) let id = fun x -> x let id2 = id"
  ; "type Foo = Foo type Bar = Bar Foo"
  ; "type Bar = Bar Foo and Foo = Foo"
  ; "type List a = Nil | Cons a (List a)"
  ; "export (id, id2) let id = fun x -> x let id2 = id id"
  ; "let unit = ()"
  ; "type Option a = None | Some a let return = Some"
  ; "type Option a = None | Some a let return = fun a -> Some a"
  ; "type Either e a = Left e | Right a let return = Right"
  ; {|type Either e a = Left e | Right a

      let rec f = fun
        | (Left e) -> e
        | (Right a) -> a

     |}
  ; {|export (map)
      type Option a = None | Some a
      let one =
        case Some 1 with
          Some x -> x
        | None -> 0

      let map = fun f opt ->
        case opt with
          Some y -> Some (f y)
        | None -> None

      let bind = fun opt f ->
        case opt with
        | Some a -> f a
        | None -> None

      let flatten = fun x ->
        case x with
        | (Some (Some b)) -> Some b
        | _ -> None
    |}
  ; {|export (x)
      type Either e a = Left e | Right a
      let x =
        case Left 1 with
        | Left _ -> 0
        | Right x -> x
     |}
  ; "type Product a b = Pair a * b let mkPair = fun x y -> Pair x y"
  (*; {|(id, const, undefined)

      let id = foreign "id" forall t. t -> t

      let const = foreign "const" forall t u . t -> u -> t

      let undefined = foreign "undefined" forall t. t

      let id2 = id id id

      let id3 = const undefined id
     |}*)
  ; {|type Option a = None | Some a

      let Some x = Some 1

      let Some y = Some ""

      let _ =
        let r = Ref None in
        r := Some 0;
        r := Some 1

      let make_ref = fun x -> Ref x

      let str_ref = make_ref ""

      let int_ref = make_ref 0

      let opt_ref = make_ref None

      let _ =
        opt_ref := Some 1;
        opt_ref := Some 2

     |}
  ; {|export (x)
      let Ref x = Ref 0
     |}
  ; {|type Option a = Some a | None

      let id = fun x -> x

      let generalize_me = id (fun x -> Ref x)

      let str_ref = generalize_me "foo"

      let int_ref = generalize_me 0
     |}
  ; {|export ()
      type Option a = Some a | None

      let generalize_me = (fun x -> x) (fun x -> Ref x)

      let str_ref = generalize_me "foo"

      let int_ref = generalize_me 0
     |}
  ; {|type RefProduct a b = Pair (Ref a) * (Ref b)

      type Option a = None | Some a

      let null_pair = Pair (Ref None) (Ref None)

      let _ =
        case null_pair with
        | Pair l r ->
          l := Some 0;
          r := Some "x"

     |}
  ; {|type Nat = Z | S Nat

      type Option a = None | Some a

      let x = None

      let _ =
        case x with
        | None -> 0
        | Some Z -> 1

      let _ =
        case x with
        | None -> 0
        | Some None -> 1

     |}
  ; {|type Nat = Z | S Nat

      let rec add = fun
        | Z y -> y
        | (S x) y -> add x (S y)

     |}
  ; {|type Option a = None | Some a

      let ref = Ref

      let x = ref None

      let _ =
        x := Some 1
     |}
  ; {|type T = C Unit

      let x = C ()

      let C () = x
     |}
  ; {|let r = Ref 1

      let () = r := 2
     |}
  ; {|type Product a b = Pair a * b

      let f = fun x y -> Pair x y

      let g = f 1

      let x = g 2

      let y = g 3
     |}
  ; {|type Bool = False | True

      let not = fun
        | False -> True
        | True -> False

      let id = fun x -> x

      let f = fun
        | False -> id
        | True -> not

      let b = f True True

      let () = case b with
        | False -> ()

     |}
  ; {|let puts = foreign "puts" forall a . a -> Unit

      let () = puts "Hello world!\n"
     |}
  ; {|type IntPair = Pair Int * Int

      let pair = Pair 0 2

     |}
  ; {|type Product a b = Pair a * b

      type Foo = Foo Product Int String

      let Pair i s = Pair 2 "foobar"
     |}
  ; {|import "std" Prelude as P

      let () = P.puts "Hello world!\n"
     |}
  ; {|import "std" Prelude as P

      let opt = P.None

      let () =
        P.puts
          (case opt with
           | P.None -> "Good"
           | P.Some _ -> "Bad")
     |} ]

let std_prelude_prefix =
  { Qual_id.Prefix.package = "std"
  ; path = ["Prelude"] }

let create_std () =
  let std = Hashtbl.create (module Qual_id.Prefix) in
  let rt = Hashtbl.create (module Qual_id.Prefix) in

  let prelude_ct, prelude_rt  =
    let code =
      {|
export (id, const, puts)

let id = fun x -> x

let const = fun x _ -> x

let puts = foreign "puts" forall a . a -> Unit

type Option a = Some a | None
       |}
    in
    match
      Parser.file Lexer.expr (Lexing.from_string code)
      |> Pipeline.compile (Hashtbl.create (module Qual_id.Prefix))
           std_prelude_prefix
    with
    | Ok(package, compiled) ->
       package, Eval.eval (Eval.create Io.stdio rt) compiled
    | Error _ -> assert false
  in
  Hashtbl.add_exn std ~key:std_prelude_prefix ~data:prelude_ct;
  Hashtbl.add_exn rt ~key:std_prelude_prefix ~data:prelude_rt;
  std, rt

let main_prefix = { Qual_id.Prefix.package = "main"; path = [] }

let () =
  List.iter ~f:(fun test ->
      let std, rt = create_std () in
      match
        Parser.file Lexer.expr (Lexing.from_string test)
        |> Pipeline.compile std main_prefix
      with
      | Ok (_, file) ->
         begin
           let ctx = Eval.create Io.stdio rt in
           try ignore (Eval.eval ctx file) with
           | _ ->
              let pp = Prettyprint.create () in
              Prettyprint.Asm.print_module pp file;
              failwith (test ^ "\n" ^ (Prettyprint.to_string pp))
         end
      | Error e ->
         let pp = Prettyprint.create () in
         Prettyprint.print_message Prettyprint.print_span pp e;
         Stdio.print_endline (Prettyprint.to_string pp);
         raise (Module_fail test)
      | exception Parser.Error ->
         raise (Module_fail test)
    ) tests

let tests =
  [ {|export (id, id2, id3)
      let rec id = fun x -> x and id2 = id let id3 = id2 id id2
    |}
  ; {|type Option a = None | Some a

      let r = Ref None

      let _ = r := Some 0

      let _ = r := Some "foo"
     |}
  ; {|type Option a = None | Some a

      let r = (fun x -> Ref x) None

      let _ =
        r := Some 0;
        r := Some "foo"
     |}
  ; {|type Option a = None | Some a

      let r = (fun _ -> Ref None) 0

      let _ =
        r := Some 0;
        r := Some "foo"
     |}
  ; {|type RefProduct a b = Pair (Ref a) * (Ref b)

      type Option a = None | Some a

      let null_pair = Pair (Ref None) (Ref None)

      let _ =
        case null_pair with
        | Pair l r ->
          l := Some 0;
          l := Some "x"

     |}
  ; {|let mkRef = foreign "ref" forall a!1 . a -> Ref a

      type Option a = None | Some a

      let x = mkRef None

      let _ = x := Some 1

      let _ = x := Some "foo"
     |}
  ; {|type T a = T (Ref a)

      type U a = U (T a)

      type V a = V (U a)

      type Option a = None | Some a

      let f = fun
        | (V (U (T r))) -> r

      let x = V (U (T (Ref None)))

      let x' = f x

      let _ =
        x' := Some ""

      let _ =
        x := Some 1

     |}
  ; {|type T a = T (Ref a)

      type U a = U (T a)

      type V a = V (U a)

      type Option a = None | Some a

      let f = fun
        | a (V (U (T r))) ->
           r := Some a

      let x = V (U (T (Ref None)))

      let _ = f 1 x

      let _ = f "" x

     |}
  ; {|type Option a = None | Some a

      let ref = Ref

      let x = ref None

      let _ =
        x := Some 1;
        x := Some ""
     |}
  ; {|type Option a = None | Some a

      let () = None
     |} ]

let () =
  List.iter ~f:(fun test ->
      match Parser.file Lexer.expr (Lexing.from_string test)
            |> Pipeline.compile (Hashtbl.create (module Qual_id.Prefix))
                 main_prefix
      with
      | Ok _ -> raise (Module_fail test)
      | Error _ -> ()
    ) tests
