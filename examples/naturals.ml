type Nat = Z | Suc Nat

let two = Suc (Suc Z)
let three = Suc two

let rec iter = fun
  | _ Z -> ()
  | f (Suc n) ->
    f ();
    iter f n

let rec add = fun
  | Z n -> n
  | (Suc m) n -> add m (Suc n)

let five = add two three

let puts = foreign "puts" forall. String -> Unit

(* Count to five *)
let () = iter (fun () -> puts "X\n") five
