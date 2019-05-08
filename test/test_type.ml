open Base
open Emmeline

let vargen = Type.create_vargen ()

let dummy_quant () = Type.Exists (ref 0)

let tvar1 =
  Type.fresh_var
    vargen Type.Pure (dummy_quant ()) 0 Kind.Mono
let tvar2 =
  Type.fresh_var
    vargen Type.Pure (dummy_quant ()) 0 Kind.Mono

let () =
  assert ((Type.Var.compare tvar1 tvar2) <> 0);
  assert ((Type.Var.compare tvar1 tvar1) = 0);
  assert ((Type.Var.compare tvar2 tvar2) = 0)

let () =
  assert (Type.occurs tvar1 (Type.Var tvar1));
  assert (Type.occurs tvar1 (Type.App(Type.Var tvar1, Type.Prim Type.Float)));
  assert (Type.occurs tvar2 (Type.App(Type.Prim Type.Arrow, Type.Var tvar2)));
  assert (not (Type.occurs tvar1 (Type.Var tvar2)))
