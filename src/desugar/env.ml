open Base

type ('k, +'v, 'cmp) t =
  { curr : ('k, 'v, 'cmp) Map.t
  ; parents : ('k, 'v, 'cmp) Map.t }

let empty cmp =
  { curr = Map.empty cmp
  ; parents = Map.empty cmp }

let of_map map =
  { curr = map
  ; parents = Map.empty (Map.comparator_s map) }

let extend parent map =
  let combine ~key:_ x _ = x in
  { curr = map
  ; parents = Map.merge_skewed parent.parents parent.curr ~combine }

let in_scope_with f frame env =
  let combine ~key:_ x _ = x in
  let env =
    { curr = frame
    ; parents = Map.merge_skewed env.curr env.parents ~combine:combine }
  in f env

let in_scope f env = in_scope_with f (Map.empty (Map.comparator_s env.curr)) env

let find env key =
  match Map.find env.curr key with
  | Some x -> Some x
  | None -> Map.find env.parents key

let add env key value =
  match Map.add env.curr ~key:key ~data:value with
  | `Ok x -> Some { env with curr = x }
  | `Duplicate -> None
