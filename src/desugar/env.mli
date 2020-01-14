(* Copyright (C) 2019-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
(** An immutable map with scoped name shadowing and O(log n) lookup *)

type ('k, +'v, 'cmp) t

val empty : ('k, 'cmp) Base.Map.comparator -> ('k, 'v, 'cmp) t

val of_map : ('k, 'v, 'cmp) Base.Map.t -> ('k, 'v, 'cmp) t

val to_map : ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) Base.Map.t

val extend : ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) Base.Map.t -> ('k, 'v, 'cmp) t

val in_scope_with :
  (('k, 'v, 'cmp) t -> 'a) ->
  ('k, 'v, 'cmp) Base.Map.t ->
  ('k, 'v, 'cmp) t ->
  'a

val in_scope : (('k, 'v, 'cmp) t -> 'a) -> ('k, 'v, 'cmp) t -> 'a

val find : ('k, 'v, 'cmp) t -> 'k -> 'v option

val add : ('k, 'v, 'cmp) t -> 'k -> 'v -> ('k, 'v, 'cmp) t option
