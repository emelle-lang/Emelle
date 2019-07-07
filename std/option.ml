export (map)

type Option a = None | Some a

let map = fun
  | _ None -> None
  | f (Some a) -> Some (f a)
