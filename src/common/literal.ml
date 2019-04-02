(* Copyright (C) 2018-2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base

type t =
  | Char of char
  | Float of float
  | Int of int
  | String of string
  | Unit
[@@deriving compare, hash, sexp]
