(* Copyright (C) 2018-2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
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

let to_map env =
  (* Prefer curr over parents *)
  let combine ~key:_ _ x = x in
  Map.merge_skewed env.parents env.curr ~combine

let extend parent map =
  { curr = map
  ; parents = to_map parent }

let in_scope_with f frame env = f (extend env frame)

let in_scope f env = in_scope_with f (Map.empty (Map.comparator_s env.curr)) env

let find env key =
  match Map.find env.curr key with
  | Some x -> Some x
  | None -> Map.find env.parents key

let add env key value =
  match Map.add env.curr ~key:key ~data:value with
  | `Ok x -> Some { env with curr = x }
  | `Duplicate -> None
