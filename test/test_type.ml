open Base
open Emmeline

let vargen = Type.create_vargen ()

let tvar1 =
  Type.fresh_wobbly vargen Type.Pure ~let_level:0 ~lam_level:0 Kind.Mono
let tvar2 =
  Type.fresh_wobbly vargen Type.Pure ~let_level:0 ~lam_level:0 Kind.Mono

let tvar1' = Type.Var (ref (Type.Wobbly tvar1))
let tvar2' = Type.Var (ref (Type.Wobbly tvar2))

let () =
  assert (Type.occurs tvar1 tvar1');
  assert (Type.occurs tvar1 (Type.App(tvar1', Type.Prim Type.Float)));
  assert (Type.occurs tvar2 (Type.App(Type.Prim Type.Arrow, tvar2')));
  assert (not (Type.occurs tvar1 tvar2'))
