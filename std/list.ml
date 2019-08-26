export (map)

type List a = Nil | Cons a * List a

let rec map = fun
  | _ Nil -> Nil
  | f (Cons x xs) -> Cons (f x) (map f xs)
