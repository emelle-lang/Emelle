type Functor f = {
  map : forall a b. (a -> b) -> f a -> f b
}

type Product a b = Pair a * b

type Applicative f = {
  functor : forall. Functor f;
  unit : forall. f Unit;
  product : forall a b. f a -> f b -> f (Product a b)
}

let map2 = fun appl f a b ->
  appl.functor.map (fun (Pair a b) -> f a b) (appl.product a b)

type Option a = None | Some a

let opt_functor = {
  map = (fun
    | _ None -> None
    | f (Some x) -> Some (f x)
  )
}

let opt_applicative = {
  functor = opt_functor;
  unit = Some ();
  product = (fun
    | (Some a) (Some b) -> Some (Pair a b)
    | _ _ -> None
  )
}
