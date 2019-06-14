(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
type t = {
    package : Package.t;
    packages : (Qual_id.Prefix.t, Package.t) Base.Hashtbl.t;
    env : (Ident.t, Type.t) Base.Hashtbl.t;
    let_level : int;
    lam_level : int;
    tvargen : Type.vargen;
    kvargen : Kind.vargen
  }

val create : Package.t -> (Qual_id.Prefix.t, Package.t) Base.Hashtbl.t -> t

val unify_kinds : Kind.t -> Kind.t -> (unit, Message.error) result

val kind_of_type : t -> Type.t -> (Kind.t, Message.error) result

val gen : t -> Type.t -> unit

val infer_term : t -> 'a Term.t -> ('a Typedtree.t, 'a Message.t) result

val typecheck : t -> 'a Term.file -> ('a Typedtree.file, 'a Message.t) result
