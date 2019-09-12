open Base
open Emmeline
(*
let vargen = Type.create_vargen ()

let tvar1 =
  Type.fresh_wobbly vargen Type.Pure ~let_level:0 ~lam_level:0 Kind.Mono
let tvar2 =
  Type.fresh_wobbly vargen Type.Pure ~let_level:0 ~lam_level:0 Kind.Mono

let tvar1' = Type.Var (ref (Type.Wobbly tvar1))
let tvar2' = Type.Var (ref (Type.Wobbly tvar2))

let assert_ok result =
  assert (match result with Ok _ -> true | Error _ -> false)

let assert_error result =
  assert (match result with Ok _ -> false | Error _ -> true)

let () =
  assert_error (Typecheck.occurs tvar1 tvar1');
  assert_error
    (Typecheck.occurs tvar1 (Type.App(tvar1', Type.Prim Type.Float)));
  assert_error
    (Typecheck.occurs tvar2 (Type.App(Type.Prim Type.Arrow, tvar2')));
  assert_ok (Typecheck.occurs tvar1 tvar2')
 *)

let monotypes =
  [ Type.Prim Type.Int
  ; Type.Prim Type.Float
  ; Type.arrow (Type.Prim Type.Int) (Type.Prim Type.Int)
  ]

let not_monotypes =
  [ Type.Prim Type.Arrow
  ; Type.App(Type.Prim Type.Arrow, Type.Prim Type.Int)
  ]

let rec is_mono = function
  | Kind.Mono -> true
  | Kind.Var { kind = Some kind; _ } -> is_mono kind
  | _ -> false

let prefix = { Qual_id.Prefix.package = ""; path = [] }

let () =
  let f x =
    let package = Package.create prefix in
    let packages = Hashtbl.create (module Qual_id.Prefix) in
    let checker = Typecheck.create package packages in
    match Typecheck.kind_of_type checker x with
    | Ok kind -> assert (is_mono kind)
    | Error _ -> assert false
  in List.iter ~f:f monotypes

let () =
  let f x =
    let package = Package.create prefix in
    let packages = Hashtbl.create (module Qual_id.Prefix) in
    let checker = Typecheck.create package packages in
    match Typecheck.kind_of_type checker x with
    | Ok kind -> assert (not (is_mono kind))
    | Error _ -> assert false
  in List.iter ~f:f not_monotypes
