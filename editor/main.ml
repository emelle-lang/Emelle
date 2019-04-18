(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Core_kernel
open Js_of_ocaml

type modul = {
    items : (symbols, items) Bexp.hole;
  }

and items =
  | ILet of ((symbols, let_def) Bexp.hole * (symbols, items) Bexp.hole)
  | ILet_rec of ((symbols, let_rec) Bexp.hole * (symbols, items) Bexp.hole)

and let_def = {
    let_pat : (symbols, pat) Bexp.hole;
    let_expr : (symbols, expr) Bexp.hole;
    let_next : (symbols, let_def) Bexp.hole;
  }

and let_rec = {
    let_rec_ident : string ref;
    let_rec_expr : (symbols, expr) Bexp.hole;
    let_rec_next : (symbols, let_rec) Bexp.hole;
  }

and expr =
  | EApp of bin_expr
  | EAssign of bin_expr
  | ECase of ((symbols, expr) Bexp.hole * (symbols, branch) Bexp.hole)
  | ELam of ((symbols, pat) Bexp.hole * (symbols, expr) Bexp.hole)
  | ELet of ((symbols, let_def) Bexp.hole * (symbols, expr) Bexp.hole)
  | ELet_rec of ((symbols, let_rec) Bexp.hole * (symbols, expr) Bexp.hole)
  | EVar of string ref

and bin_expr = (symbols, expr) Bexp.hole * (symbols, expr) Bexp.hole

and pat =
  | PConstr of (string ref * (symbols, pat_list) Bexp.hole)
  | PUnit
  | PVar of string ref
  | PWild

and pat_list =
  PList of ((symbols, pat) Bexp.hole * (symbols, pat_list) Bexp.hole)

and branch = {
    branch_pat : (symbols, pat) Bexp.hole;
    branch_expr : (symbols, expr) Bexp.hole;
    branch_next : (symbols, branch) Bexp.hole;
  }

and symbols =
  | Module of (symbols, modul) Bexp.term
  | Items of (symbols, items) Bexp.term
  | Let_def of (symbols, let_def) Bexp.term
  | Let_rec of (symbols, let_rec) Bexp.term
  | Expr of (symbols, expr) Bexp.term
  | Pat of (symbols, pat) Bexp.term
  | Pat_list of (symbols, pat_list) Bexp.term
  | Branch of (symbols, branch) Bexp.term

let doc = Dom_svg.document

let get_module = function
  | Module m -> Some m
  | _ -> None

let get_items = function
  | Items i -> Some i
  | _ -> None

let get_let_def = function
  | Let_def l -> Some l
  | _ -> None

let get_let_rec = function
  | Let_rec l -> Some l
  | _ -> None

let get_expr = function
  | Expr a -> Some a
  | _ -> None

let get_pat = function
  | Pat a -> Some a
  | _ -> None

let get_pat_list = function
  | Pat_list a -> Some a
  | _ -> None

let get_branch = function
  | Branch a -> Some a
  | _ -> None

let symbol_of_modul m = Module m

let symbol_of_expr t = Expr t

let symbol_of_items i = Items i

let symbol_of_let_def l = Let_def l

let symbol_of_let_rec l = Let_rec l

let symbol_of_pat p = Pat p

let symbol_of_pat_list p = Pat_list p

let symbol_of_branch b = Branch b

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

let module_data =
  { Bexp.palette_name = "Module"
  ; palette_color = "orange" }

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
  ; palette_color = "red" }

let pat_data =
  { Bexp.palette_name = "Pattern"
  ; palette_color = "blue" }

let pat_list_data =
  { Bexp.palette_name = "Patterns"
  ; palette_color = "green" }

let branch_data =
  { Bexp.palette_name = "Match Case"
  ; palette_color = "pink" }

let ctx =
  Bexp.create ~x:0.0 ~y:0.0 ~width ~height
    (Bexp.Hole.create get_module module_data)

let setter r str =
  r := str;
  str

let module_def =
  let open Bexp.Syntax in
  create [ text "module"; text "where"; newline; nt (fun x -> x) items_data ]
    ~create:(fun () -> Bexp.Hole.create get_items items_data)
    ~to_term:(fun items -> { items })
    ~symbol_of_term:symbol_of_modul

let ilet_def =
  let open Bexp.Syntax in
  create [ text "let"; nt left let_def_data; newline; nt right items_data ]
    ~create:(fun () ->
      ( Bexp.Hole.create get_let_def let_def_data
      , Bexp.Hole.create get_items items_data ))
    ~to_term:(fun let_def -> ILet let_def)
    ~symbol_of_term:symbol_of_items

let ilet_rec =
  let open Bexp.Syntax in
  create [ text "let rec"; nt left let_rec_data; newline; nt right items_data ]
    ~create:(fun () ->
      ( Bexp.Hole.create get_let_rec let_rec_data
      , Bexp.Hole.create get_items items_data ))
    ~to_term:(fun let_rec -> ILet_rec let_rec)
    ~symbol_of_term:symbol_of_items

let let_def_def =
  let open Bexp.Syntax in
  create
    [ nt (fun { let_pat; _ } -> let_pat) pat_data
    ; text "="
    ; nt (fun { let_expr; _ } -> let_expr) expr_data
    ; text "and"
    ; newline
    ; nt (fun { let_next; _ } -> let_next) let_def_data ]
    ~create:(fun () ->
      { let_pat = Bexp.Hole.create get_pat pat_data
      ; let_expr = Bexp.Hole.create get_expr expr_data
      ; let_next = Bexp.Hole.create get_let_def let_def_data })
    ~to_term:(fun x -> x)
    ~symbol_of_term:symbol_of_let_def

let let_rec_def =
  let open Bexp.Syntax in
  let ident = ref "x" in
  create
    [ text_input ~str:"x" (setter ident)
    ; text "="
    ; nt (fun { let_rec_expr; _ } -> let_rec_expr) expr_data
    ; text "and"
    ; newline
    ; nt (fun { let_rec_next; _ } -> let_rec_next) let_rec_data ]
    ~create:(fun () ->
      { let_rec_ident = ident
      ; let_rec_expr = Bexp.Hole.create get_expr expr_data
      ; let_rec_next = Bexp.Hole.create get_let_rec let_rec_data })
    ~to_term:(fun x -> x)
    ~symbol_of_term:symbol_of_let_rec

let evar_def =
  let open Bexp.Syntax in
  let str_ref = ref "x" in
  create [ text_input ~str:"x" (setter str_ref) ]
    ~create:(fun () -> str_ref)
    ~to_term:(fun name -> EVar name)
    ~symbol_of_term:symbol_of_expr

let eapp_def =
  let open Bexp.Syntax in
  create [nt left expr_data; text "("; nt right expr_data; text ")"]
    ~create:(fun () -> ( Bexp.Hole.create get_expr expr_data
                       , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> EApp args)
    ~symbol_of_term:symbol_of_expr

let eassn_def =
  let open Bexp.Syntax in
  create [nt left expr_data; text ":="; nt right expr_data]
    ~create:(fun () -> ( Bexp.Hole.create get_expr expr_data
                       , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> EAssign args)
    ~symbol_of_term:symbol_of_expr

let ecase_def =
  let open Bexp.Syntax in
  create
    [ text "case"; nt left expr_data; text "of"; newline; nt right branch_data ]
    ~create:(fun () -> ( Bexp.Hole.create get_expr expr_data
                       , Bexp.Hole.create get_branch branch_data ))
    ~to_term:(fun args -> ECase args)
    ~symbol_of_term:symbol_of_expr

let elam_def =
  let open Bexp.Syntax in
  create [ text "fun"; nt left pat_data; text "->"; nt right expr_data ]
    ~create:(fun () -> ( Bexp.Hole.create get_pat pat_data
                       , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> ELam args)
    ~symbol_of_term:symbol_of_expr

let elet_def =
  let open Bexp.Syntax in
  create
    [ text "let"; nt left let_def_data; text "in"; newline; nt right expr_data ]
    ~create:(fun () -> ( Bexp.Hole.create get_let_def let_def_data
                       , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> ELet args)
    ~symbol_of_term:symbol_of_expr

let elet_rec =
  let open Bexp.Syntax in
  create
    [ text "let rec"; nt left let_rec_data; text "in"
    ; newline; nt right expr_data ]
    ~create:(fun () -> ( Bexp.Hole.create get_let_rec let_rec_data
                       , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> ELet_rec args)
    ~symbol_of_term:symbol_of_expr

let pconstr_def =
  let open Bexp.Syntax in
  let str_ref = ref "Constr" in
  create [ text_input ~str:"Constr" (setter str_ref); nt right pat_list_data ]
    ~create:(fun () -> (str_ref, Bexp.Hole.create get_pat_list pat_list_data))
    ~to_term:(fun x -> PConstr x)
    ~symbol_of_term:symbol_of_pat

let punit_def =
  let open Bexp.Syntax in
  create [text "()"]
    ~create:(fun () -> ())
    ~to_term:(fun () -> PWild)
    ~symbol_of_term:symbol_of_pat

let pvar_def =
  let open Bexp.Syntax in
  let str_ref = ref "x" in
  create [text_input ~str:"x" (setter str_ref)]
    ~create:(fun () -> str_ref)
    ~to_term:(fun r -> PVar r)
    ~symbol_of_term:symbol_of_pat

let pwild_def =
  let open Bexp.Syntax in
  create [text "_"]
    ~create:(fun () -> ())
    ~to_term:(fun () -> PWild)
    ~symbol_of_term:symbol_of_pat

let plist_cons_def =
  let open Bexp.Syntax in
  create [nt left pat_data; text ","; nt right pat_list_data]
    ~create:(fun () ->
      ( Bexp.Hole.create get_pat pat_data
      , Bexp.Hole.create get_pat_list pat_list_data ))
    ~to_term:(fun args -> PList args)
    ~symbol_of_term:symbol_of_pat_list

let branch_def =
  let open Bexp.Syntax in
  create
    [ nt (fun { branch_pat; _ } -> branch_pat) pat_data
    ; text "->"
    ; nt (fun { branch_expr; _ } -> branch_expr) expr_data
    ; newline
    ; nt (fun { branch_next; _ } -> branch_next) branch_data ]
    ~create:(fun () ->
      { branch_pat = Bexp.Hole.create get_pat pat_data
      ; branch_expr = Bexp.Hole.create get_expr expr_data
      ; branch_next = Bexp.Hole.create get_branch branch_data })
    ~to_term:(fun x -> x)
    ~symbol_of_term:symbol_of_branch

let branch_palette =
  Bexp.Palette.create ctx.Bexp.workspace None
    branch_data
    [ Bexp.Syntax branch_def ]

let pat_list_palette =
  Bexp.Palette.create ctx.Bexp.workspace (Some (Palette branch_palette))
    pat_list_data
    [ Bexp.Syntax plist_cons_def ]

let pat_palette =
  Bexp.Palette.create ctx.Bexp.workspace (Some (Palette pat_list_palette))
    pat_data
    [ Bexp.Syntax pconstr_def
    ; Bexp.Syntax punit_def
    ; Bexp.Syntax pvar_def
    ; Bexp.Syntax pwild_def ]

let expr_palette =
  Bexp.Palette.create ctx.Bexp.workspace (Some (Palette pat_palette))
    expr_data
    [ Bexp.Syntax eapp_def
    ; Bexp.Syntax eassn_def
    ; Bexp.Syntax ecase_def
    ; Bexp.Syntax elam_def
    ; Bexp.Syntax elet_def
    ; Bexp.Syntax elet_rec
    ; Bexp.Syntax evar_def ]

let let_rec_palette =
  Bexp.Palette.create ctx.Bexp.workspace (Some (Palette expr_palette))
    let_rec_data [ Bexp.Syntax let_rec_def ]

let let_def_palette =
  Bexp.Palette.create ctx.Bexp.workspace (Some (Palette let_rec_palette))
    let_def_data [ Bexp.Syntax let_def_def ]

let items_palette =
  Bexp.Palette.create ctx.Bexp.workspace (Some (Palette let_def_palette))
    items_data [ Bexp.Syntax ilet_def; Bexp.Syntax ilet_rec ]

let module_palette =
  Bexp.Palette.create ctx.Bexp.workspace (Some (Palette items_palette))
    module_data [ Bexp.Syntax module_def ]

let () =
  Bexp.Toolbox.set_palette ctx.Bexp.workspace.toolbox module_palette;
  ignore (svg##appendChild
            (ctx.Bexp.workspace.root_layer#element :> Dom.node Js.t));
  Bexp.Workspace.render ctx.Bexp.workspace
