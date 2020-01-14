type List a = Nil | Cons a * (List a)

let rec iter = fun
  | _ Nil -> ()
  | f (Cons x xs) ->
     f x;
     iter f xs

let puts = foreign "puts" forall . String -> Unit

let () = iter puts (Cons "1\n" (Cons "2\n" (Cons "3\n" Nil)))
