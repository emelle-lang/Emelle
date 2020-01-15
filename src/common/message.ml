(* Copyright (C) 2018-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

type error =
  | Abstract_type of Qual_id.t
  | And_error of error * error
  | Different_records of string * Qual_id.t * string * Qual_id.t
  | Escaping_rigid of Type.rigid_var
  | Kind_unification_fail of Kind.t * Kind.t
  | Lexer_error of string
  | Mismatched_arity
  | Missing_field of string
  | Not_enough_fields
  | Occurs of Type.wobbly_var * Type.t
  | Parser_error
  | Redefined_constr of string
  | Redefined_field of string
  | Redefined_name of string
  | Redefined_typevar of string
  | Reexported_name of string
  | Redefined_field_def of string
  | Syntax_error
  | Too_many_fields
  | Type_unification_fail of Type.t * Type.t
  | Typed_hole of (string, Ident.t, String.comparator_witness) Env.t
                  * (Ident.t, Type.polytype) Hashtbl.t * Type.t
  | Unimplemented of string
  | Unknown_constr of Qual_id.t * string
  | Unreachable_error of string
  | Unresolved_field of string
  | Unresolved_id of Qual_id.t
  | Unresolved_name of string
  | Unresolved_path of Ast.qual_id
  | Unresolved_prefix of Qual_id.Prefix.t
  | Unresolved_type of Qual_id.t
  | Unresolved_typevar of string
  | Unsafe_let_rec

type 'a diagnostic = {
    loc : 'a;
    error : error;
  }

type 'a t =
  | And of 'a t * 'a t
  | Diagnostic of 'a diagnostic
  | Unreachable of string

let error loc error =
  Error (Diagnostic { loc; error })

let at loc = function
  | Ok x -> Ok x
  | Error e -> error loc e

let unreachable str = Error (Unreachable str)
