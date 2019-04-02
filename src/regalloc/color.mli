(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
type coloring = {
    map : (Ir.Register.t, int) Base.Hashtbl.t;
    frame_size : int;
  }

type t = {
    colorings : (int, coloring, Base.Int.comparator_witness) Base.Map.t;
    main's_coloring : coloring;
  }

val handle_file : Ssa2.file -> (t, 'a Message.t) result
