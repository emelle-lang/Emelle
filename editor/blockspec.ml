(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

module T =
  [%symbol
   type ident =
     | Local of Bexp.Widget.text_input
     | Qual of Bexp.Widget.text_input * Bexp.Widget.text_input

   type imports = {
       import_package : Bexp.Widget.text_input;
       import_path : Bexp.Widget.text_input;
       import_alias : Bexp.Widget.text_input;
       import_next : (symbols, imports) Bexp.hole;
     }

   and items =
     | ILet of (symbols, let_def) Bexp.hole * (symbols, items) Bexp.hole
     | ILet_rec of (symbols, let_rec) Bexp.hole * (symbols, items) Bexp.hole
     | ITypedefs of (symbols, typedef) Bexp.hole * (symbols, items) Bexp.hole

   and modul = {
       imports : (symbols, imports) Bexp.hole;
       items : (symbols, items) Bexp.hole;
     }

   and let_def = {
       let_pat : (symbols, pat) Bexp.hole;
       let_expr : (symbols, expr) Bexp.hole;
       let_next : (symbols, let_def) Bexp.hole;
     }

   and let_rec = {
       let_rec_ident : Bexp.Widget.text_input;
       let_rec_expr : (symbols, expr) Bexp.hole;
       let_rec_next : (symbols, let_rec) Bexp.hole;
     }

   and expr =
     | EApp of bin_expr
     | EAssign of bin_expr
     | ECase of ((symbols, expr) Bexp.hole * (symbols, branch) Bexp.hole)
     | EConstr of (symbols, ident) Bexp.hole
     | ELam of ((symbols, pat) Bexp.hole * (symbols, expr) Bexp.hole)
     | ELet of ((symbols, let_def) Bexp.hole * (symbols, expr) Bexp.hole)
     | ELet_rec of ((symbols, let_rec) Bexp.hole * (symbols, expr) Bexp.hole)
     | ERef
     | ESeq of bin_expr
     | EString of Bexp.Widget.text_input
     | EUnit
     | EVar of (symbols, ident) Bexp.hole

   and bin_expr = (symbols, expr) Bexp.hole * (symbols, expr) Bexp.hole

   and pat =
     | PConstr of ((symbols, ident) Bexp.hole * (symbols, pat_list) Bexp.hole)
     | PRef of (symbols, pat) Bexp.hole
     | PUnit
     | PVar of Bexp.Widget.text_input
     | PWild

   and pat_list =
     PList of ((symbols, pat) Bexp.hole * (symbols, pat_list) Bexp.hole)

   and branch = {
       branch_pat : (symbols, pat) Bexp.hole;
       branch_expr : (symbols, expr) Bexp.hole;
       branch_next : (symbols, branch) Bexp.hole;
     }

   and typedef =
     | Adt of Bexp.Widget.text_input
              * (symbols, constr) Bexp.hole
              * (symbols, typedef) Bexp.hole

   and constr = {
       constr_name : Bexp.Widget.text_input;
       constr_ty : (symbols, product_ty) Bexp.hole;
       constr_next : (symbols, constr) Bexp.hole;
    }

   and product_ty = {
       prod_factor : (symbols, ty) Bexp.hole;
       prod_next : (symbols, product_ty) Bexp.hole;
     }

   and ty =
     | TApp of (symbols, ty) Bexp.hole * (symbols, ty) Bexp.hole
     | TArr
     | TNominal of (symbols, ident) Bexp.hole
     | TRef
     | TVar of Bexp.Widget.text_input
  ]

include T

let ident_data =
  { Bexp.palette_name = "Name"
  ; palette_color = "purple" }

let modul_data =
  { Bexp.palette_name = "Module"
  ; palette_color = "lime" }

let imports_data =
  { Bexp.palette_name = "Import"
  ; palette_color = "darkseagreen" }

let items_data =
  { Bexp.palette_name = "Item"
  ; palette_color = "brown" }

let let_def_data =
  { Bexp.palette_name = "Let Defn"
  ; palette_color = "turquoise" }

let let_rec_data =
  { Bexp.palette_name = "Rec Defn"
  ; palette_color = "maroon" }

let expr_data =
  { Bexp.palette_name = "Expression"
  ; palette_color = "orange" }

let pat_data =
  { Bexp.palette_name = "Pattern"
  ; palette_color = "blue" }

let pat_list_data =
  { Bexp.palette_name = "Patterns..."
  ; palette_color = "green" }

let branch_data =
  { Bexp.palette_name = "Match Cases..."
  ; palette_color = "pink" }

let ty_data =
  { Bexp.palette_name = "Type"
  ; palette_color = "grey" }

let product_ty_data =
  { Bexp.palette_name = "Product Type"
  ; palette_color = "green" }

let constr_data =
  { Bexp.palette_name = "Sum Type"
  ; palette_color = "green" }

let typedef_data =
  { Bexp.palette_name = "Typedef"
  ; palette_color = "green" }
