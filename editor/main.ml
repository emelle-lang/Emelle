(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Core_kernel
open Js_of_ocaml

type expr =
  | EApp of bin_expr
  | EAssign of bin_expr
  | ELam of ((symbols, pat) Bexp.hole * (symbols, expr) Bexp.hole)
  | ECase of (symbols, expr) Bexp.hole
  | EVar of string ref

and bin_expr = (symbols, expr) Bexp.hole * (symbols, expr) Bexp.hole

and pat =
  | PConstr of (string ref * (symbols, pat_list) Bexp.hole)
  | PVar of string ref
  | PWild

and pat_list =
  PList of ((symbols, pat) Bexp.hole * (symbols, pat_list) Bexp.hole)

and symbols =
  | Expr of (symbols, expr) Bexp.term
  | Pat of (symbols, pat) Bexp.term
  | Pat_list of (symbols, pat_list) Bexp.term

let doc = Dom_svg.document

let get_expr = function
  | Expr a -> Some a
  | _ -> None

let get_pat = function
  | Pat a -> Some a
  | _ -> None

let get_pat_list = function
  | Pat_list a -> Some a
  | _ -> None

let symbol_of_expr t = Expr t

let symbol_of_pat p = Pat p

let symbol_of_pat_list p = Pat_list p

let left (l, _) = l

let right (_, r) = r

let svg =
  match
    Dom_svg.getElementById "workspace"
    |> Dom_svg.CoerceTo.svg
    |> Js.Opt.to_option
  with
  | None -> assert false
  | Some svg -> svg

let width = Bexp.Widget.length_of_anim svg##.width

let height = Bexp.Widget.length_of_anim svg##.height

let ctx =
  Bexp.Workspace.create ~x:0.0 ~y:0.0 ~width ~height

let expr_data =
  { Bexp.palette_name = "Expression"
  ; Bexp.palette_color = "red" }

let pat_data =
  { Bexp.palette_name = "Pattern"
  ; Bexp.palette_color = "blue" }

let pat_list_data =
  { Bexp.palette_name = "Patterns"
  ; Bexp.palette_color = "green" }

let setter r str =
  r := str;
  str

let evar_def =
  let open Bexp.Syntax in
  let str_ref = ref "x" in
  Bexp.Syntax.create [ text_input ~str:"x" (setter str_ref) ]
    ~create:(fun () -> str_ref)
    ~to_term:(fun name -> EVar name)
    ~symbol_of_term:symbol_of_expr

let eapp_def =
  let open Bexp.Syntax in
  Bexp.Syntax.create [nt left expr_data; text "("; nt right expr_data; text ")"]
    ~create:(fun () -> ( Bexp.Hole.create get_expr expr_data
                       , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> EApp args)
    ~symbol_of_term:symbol_of_expr

let eassn_def =
  let open Bexp.Syntax in
  Bexp.Syntax.create [nt left expr_data; text ":="; nt right expr_data]
    ~create:(fun () -> ( Bexp.Hole.create get_expr expr_data
                       , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> EAssign args)
    ~symbol_of_term:symbol_of_expr

let elam_def =
  let open Bexp.Syntax in
  Bexp.Syntax.create
    [ text "fun"; nt left pat_data; text "->"; nt right expr_data ]
    ~create:(fun () -> ( Bexp.Hole.create get_pat pat_data
                       , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> ELam args)
    ~symbol_of_term:symbol_of_expr

let pconstr_def =
  let open Bexp.Syntax in
  let str_ref = ref "Constr" in
  Bexp.Syntax.create
    [ text_input ~str:"Constr" (setter str_ref); nt right pat_list_data ]
    ~create:(fun () -> (str_ref, Bexp.Hole.create get_pat_list pat_list_data))
    ~to_term:(fun x -> PConstr x)
    ~symbol_of_term:symbol_of_pat

let pvar_def =
  let open Bexp.Syntax in
  let str_ref = ref "x" in
  Bexp.Syntax.create [text_input ~str:"x" (setter str_ref)]
    ~create:(fun () -> str_ref)
    ~to_term:(fun r -> PVar r)
    ~symbol_of_term:symbol_of_pat

let pwild_def =
  let open Bexp.Syntax in
  Bexp.Syntax.create [text "_"]
    ~create:(fun () -> ())
    ~to_term:(fun () -> PWild)
    ~symbol_of_term:symbol_of_pat

let plist_cons_def =
  let open Bexp.Syntax in
  Bexp.Syntax.create [nt left pat_data; text ","; nt right pat_list_data]
    ~create:(fun () -> ( Bexp.Hole.create get_pat pat_data
                       , Bexp.Hole.create get_pat_list pat_list_data ))
    ~to_term:(fun args -> PList args)
    ~symbol_of_term:symbol_of_pat_list

let pat_list_palette =
  Bexp.Palette.create ctx None
    pat_list_data
    [ Bexp.Syntax plist_cons_def ]

let pat_palette =
  Bexp.Palette.create ctx (Some (Palette pat_list_palette))
    pat_data
    [ Bexp.Syntax pconstr_def
    ; Bexp.Syntax pvar_def
    ; Bexp.Syntax pwild_def ]

let expr_palette =
  Bexp.Palette.create ctx (Some (Palette pat_palette))
    expr_data
    [ Bexp.Syntax eapp_def
    ; Bexp.Syntax eassn_def
    ; Bexp.Syntax elam_def
    ; Bexp.Syntax evar_def ]

let () =
  Bexp.Toolbox.set_palette ctx.Bexp.toolbox expr_palette

let () =
  ignore (svg##appendChild (ctx.Bexp.root_layer#element :> Dom.node Js.t));
  Bexp.Workspace.render ctx
