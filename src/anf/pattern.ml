(* Copyright (C) 2018-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base

type 'a t = {
    ann : 'a;
    node : 'a pattern;
    id : Ident.t option;
  }

and 'a pattern =
  | Con of Type.adt * int * 'a t list (** Constructor pattern *)
  | Deref of 'a t
  | Or of 'a t * 'a t
  | Unit
  | Wild (** Wildcard pattern *)

type 'a row = {
    patterns : 'a t list;
    bindings : (Ident.t, Anf.operand, Ident.comparator_witness) Map.t;
    (** [bindings] holds a map from idents to already-popped occurrences. *)
    action : int
  }

(** Invariant: All rows in the matrix have the same length *)
type 'a matrix = 'a row list

type context = {
    reg_gen : Ir.Register.gen
  }

let create reg_gen =
  { reg_gen }

let fresh_reg ctx =
  Ir.Register.fresh ctx.reg_gen

(** Read into a row, and returns Some row where the indexed pattern has been
    moved to the front, or None if the index reads out of bounds. *)
let swap_column_of_row (idx : int) (row : 'a row) =
  let rec f idx left = function
    | pivot :: right when idx = 0 -> Some (left, pivot, right)
    | x :: next -> f (idx - 1) (x :: left) next
    | [] -> None in
  match f idx [] row.patterns with
  | Some (left, pivot, right) ->
     Some { row with patterns = pivot :: (List.rev_append left right) }
  | None -> None

(** Column-swapping operation for matrices *)
let swap_column idx =
  let open Option.Monad_infix in
  List.fold_right ~f:(fun row acc ->
      acc >>= fun rows ->
      swap_column_of_row idx row >>| fun row -> row :: rows
    ) ~init:(Some [])

type find_adt_result =
  | All_wilds
  | Found_adt of Type.adt * int
  | Found_ref of int

let find_adt pats =
  let rec f i = function
    | [] -> All_wilds
    | { node = Con(adt, _, _); _ } :: _ -> Found_adt(adt, i)
    | { node = Deref _; _ } :: _ -> Found_ref i
    | { node = Unit | Wild; _ } :: xs -> f (i + 1) xs
    | { node = Or(p1, _); _ } :: pats -> f i (p1 :: pats)
  in f 0 pats

(** Specialize operation as described in Compiling Pattern Matching to Good
    Decision Trees *)
let rec specialize constr product occurrence rows =
  let open Option.Monad_infix in
  let helper row rows =
    match row.patterns with
    | [] -> None
    | first_pat :: rest_pats ->
       let bindings =
         match first_pat.id with
         | None -> row.bindings
         | Some id -> Map.set row.bindings ~key:id ~data:occurrence
       in
       let rec fill acc next = function
         | [] -> acc
         | _ :: xs -> fill (next :: acc) next xs
       in
       match first_pat.node with
       | Con(_, name, cpats) when name = constr ->
          Some ({ row with
                  patterns = cpats@rest_pats
                ; bindings } :: rows)
       | Con _ -> Some rows
       | Deref _ -> None
       | Unit | Wild ->
          Some ({ row with
                  patterns =
                    fill rest_pats
                      { ann = first_pat.ann; node = Wild; id = None } product
                ; bindings } :: rows)
       | Or(p1, p2) ->
          specialize constr product occurrence
            [{ row with
               patterns = p1 :: rest_pats
             ; bindings }]
          >>= fun mat1 ->
          specialize constr product occurrence
            [{ row with
               patterns = p2 :: rest_pats
             ; bindings }]
          >>| fun mat2 ->
          mat1@mat2@rows
  in
  List.fold_right ~f:(fun row acc ->
      acc >>= fun rows -> helper row rows
    ) ~init:(Some []) rows

(** A variation of the specialization algorithm for dereference patterns *)
let specialize_ref occurrence rows =
  let open Option.Monad_infix in
  let helper row rows =
    match row.patterns with
    | [] -> None
    | { node; id; ann } :: rest_pats ->
       let bindings =
         match id with
         | None -> row.bindings
         | Some id -> Map.set row.bindings ~key:id ~data:occurrence in
       match node with
       | Deref pat->
          Some ({ row with
                  patterns = pat :: rest_pats
                ; bindings } :: rows)
       | Unit | Wild ->
          Some ({ row with
                  patterns = { ann; node = Wild; id = None } :: rest_pats
                ; bindings } :: rows)
       | _ -> None
  in
  List.fold_right ~f:(fun row acc ->
      acc >>= fun rows -> helper row rows
    ) ~init:(Some []) rows

(** Construct the default matrix *)
let rec default_matrix rows =
  let open Option.Monad_infix in
  let helper row rows =
    match row.patterns with
    | [] -> None
    | first_pat::rest_pats ->
       match first_pat.node with
       | Con _ -> Some rows
       | Deref _ -> None
       | Unit | Wild -> Some ({ row with patterns = rest_pats } :: rows)
       | Or(p1, p2) ->
          default_matrix [{ row with patterns = p1 :: rest_pats }]
          >>= fun mat1 ->
          default_matrix [{ row with patterns = p2 :: rest_pats }]
          >>| fun mat2 ->
          mat1@mat2@rows
  in
  List.fold_right ~f:(fun row acc ->
      acc >>= fun rows -> helper row rows
    ) ~init:(Some []) rows

let map_ids_to_occs occurrences row =
  let rec go bindings occurrences list =
    match occurrences, list with
    | [], [] -> Ok bindings
    | [], _ :: _ -> Error (Message.Unreachable_error "Too many patterns")
    | _ :: _, [] -> Error (Message.Unreachable_error "Too many occurrences")
    | occ :: occs, pat :: pats ->
       match pat.id with
       | None -> go bindings occs pats
       | Some id -> go (Map.set bindings ~key:id ~data:occ) occs pats
  in go row.bindings occurrences row.patterns

(** Corresponds with swap_columns and swap_column_of_row (The occurrences vector
    is like another row) *)
(* Maybe make function generic instead of repeating code? TODO consider later *)
let swap_occurrences idx occurrences =
  let rec f idx left = function
    | pivot :: right when idx = 0 -> Some (left, pivot, right)
    | x :: next -> f (idx - 1) (x :: left) next
    | [] -> None in
  match f idx [] occurrences with
  | Some (left, pivot, right) -> Some (pivot, List.rev_append left right)
  | None -> None

(** Compilation scheme CC *)
let rec decision_tree_of_matrix ctx (occurrences : Anf.operand list) =
  let open Result.Monad_infix in
  function
  | [] -> Ok Anf.Fail (* Case 1 *)
  | (row :: _) as rows ->
     match find_adt row.patterns with
     | All_wilds ->
        (* Case 2 *)
        map_ids_to_occs occurrences row >>| fun map ->
        Anf.Leaf(Map.data map, row.action)
     | Found_adt(alg, i) ->
        (* Case 3 *)
        begin match swap_column i rows with
        | None -> Error (Message.Unreachable_error "")
        | Some rows ->
           match default_matrix rows with
           | None ->
              Error (Message.Unreachable_error "No default of empty matrix")
           | Some default ->
              match swap_occurrences i occurrences with
              | None ->
                 Error (Message.Unreachable_error
                          ("Pattern idx out of bounds: " ^ (Int.to_string i)))
              | Some (first_occ, rest_occs) ->
                 let tag_reg = fresh_reg ctx in
                 Array.foldi
                   alg.Type.datacons
                   ~init:(Ok (Map.empty (module Int)))
                   ~f:(fun id acc (_, _, product, _) ->
                     acc >>= fun jump_tbl ->
                     (* Just like how the matched value is popped off the stack
                        and its children pushed on the stack, pop off the
                        selected occurrence and push occurrences for its
                        subterms *)
                     let rec push_occs rest = function
                       | [] -> [], rest
                       | _ :: xs ->
                          let reg = fresh_reg ctx in
                          let (new_regs, occs) = push_occs rest xs in
                          reg :: new_regs, (Ir.Operand.Register reg) :: occs in
                     let (new_regs, pushed_occs) =
                       push_occs rest_occs product in
                     match specialize id product first_occ rows with
                     | None -> Error (Message.Unreachable_error "dec tree 1")
                     | Some matrix ->
                        match
                          decision_tree_of_matrix ctx pushed_occs matrix
                        with
                        | Error e -> Error e
                        | Ok tree ->
                           match
                             Map.add jump_tbl ~key:id ~data:(new_regs, tree)
                           with
                           | `Ok jump_tbl -> Ok jump_tbl
                           | `Duplicate ->
                              Error (Message.Unreachable_error "dec tree dup")
                   )
                 >>= fun jump_tbl ->
                 decision_tree_of_matrix ctx rest_occs default
                 >>| fun default_tree ->
                 Anf.Switch(tag_reg, first_occ, jump_tbl, default_tree)
        end
     | Found_ref i ->
        match swap_occurrences i occurrences with
        | None -> Error (Message.Unreachable_error "")
        | Some (first_occ, rest_occs) ->
           match swap_column i rows with
           | None -> Error (Message.Unreachable_error "")
           | Some rows ->
              match specialize_ref first_occ rows with
              | None -> Error (Message.Unreachable_error "")
              | Some matrix ->
                 let reg = fresh_reg ctx in
                 let occs = (Ir.Operand.Register reg)::rest_occs in
                 decision_tree_of_matrix ctx occs matrix >>| fun tree ->
                 Anf.Deref(first_occ, reg, tree)
