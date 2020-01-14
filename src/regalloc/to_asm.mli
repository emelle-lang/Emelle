(* Copyright (C) 2019-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

val compile : Color.t -> Ssa2.file -> (Asm.file, 'a Message.t) result
