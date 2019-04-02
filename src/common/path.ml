(* Copyright (C) 2018-2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
(** The module of fully-qualified identifers *)

open Base

module T = struct
  type t = string * string
  [@@deriving compare, hash, sexp]

  let equal x y = (compare x y) = 0
end

include T
include Comparator.Make(T)
