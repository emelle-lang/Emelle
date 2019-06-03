(* Copyright (C) 2018-2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

(** A processed compilation unit:
    - Maps type constructor names to type declarations (abstract types or ADTs)
    - Maps data constructor names to ADTs and the integer index
    - Maps variable names to types *)

open Base

type ty_state =
  | Compiled of Type.decl
  | Todo of Kind.t
  | Prim of Type.prim

type t = {
    prefix : Qual_id.Prefix.t;
    typedefs : (string, ty_state ref) Hashtbl.t;
    datacons : (string, Type.adt * int) Hashtbl.t;
    vals : (string, Type.t * int) Hashtbl.t
  }

let create prefix =
  { prefix
  ; typedefs =
      Hashtbl.of_alist_exn
        (module String)
        [ "Char", ref (Prim Type.Char)
        ; "Float", ref (Prim Type.Float)
        ; "Int", ref (Prim Type.Int)
        ; "String", ref (Prim Type.String)
        ; "Unit", ref (Prim Type.Unit) ]
  ; datacons = Hashtbl.create (module String)
  ; vals = Hashtbl.create (module String) }

let find f self name = Hashtbl.find (f self) name

let find_typedef = find (fun package -> package.typedefs)

let find_adt = find (fun package -> package.datacons)

let find_val = find (fun package -> package.vals)

let kind_of_ident self name =
  match find_typedef self name with
  | None -> None
  | Some ptr ->
     match !ptr with
     | Compiled (Type.Manifest adt) -> Some (Type.kind_of_adt adt)
     | Compiled (Type.Abstract kind) -> Some kind
     | Todo kind -> Some kind
     | Prim prim -> Some (Type.kind_of_prim prim)

let add_typedef self name typedef =
  match Hashtbl.add self.typedefs ~key:name ~data:(ref typedef) with
  | `Ok -> Ok ()
  | `Duplicate -> Error (Message.Redefined_name name)

let add_datacons self adt =
  let open Result.Monad_infix in
  Hashtbl.fold
    ~f:(fun ~key:constr ~data:idx acc ->
      acc >>= fun () ->
      match Hashtbl.add self.datacons ~key:constr ~data:(adt, idx) with
      | `Ok -> Ok ()
      | `Duplicate -> Error (Message.Redefined_constr constr)
    ) ~init:(Ok ()) adt.Type.datacon_names

let add_val self name ty reg =
  match Hashtbl.add self.vals ~key:name ~data:(ty, reg) with
  | `Ok -> Some ()
  | `Duplicate -> None
