open Base
open Emmeline

let create_canvas () =
  `Canvas

let add_create_canvas t =
  Eval.add_foreign_fun t "create_canvas"
    (Eval.foreign ~arity:1 (function
         | [|`Unit|] -> `Canvas
         | _ -> failwith "Type error" ))

let add vm =
  add_create_canvas vm
