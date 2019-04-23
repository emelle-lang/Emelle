(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Core_kernel
open Js_of_ocaml
open Emelle

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
    let_rec_ident : Bexp.Widget.text_input;
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
  | ESeq of bin_expr
  | EUnit
  | EVar of Bexp.Widget.text_input

and bin_expr = (symbols, expr) Bexp.hole * (symbols, expr) Bexp.hole

and pat =
  | PConstr of (Bexp.Widget.text_input * (symbols, pat_list) Bexp.hole)
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
  { Bexp.palette_name = "Patterns..."
  ; palette_color = "green" }

let branch_data =
  { Bexp.palette_name = "Match Cases..."
  ; palette_color = "pink" }

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
  Bexp.create ~x:0.0 ~y:0.0 ~width ~height
    (Bexp.Hole.create get_module module_data)

let id x = x

let module_def =
  let open Bexp.Syntax in
  create [ text "module"; text "where"; newline; nt id items_data ]
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

let ilet_rec_def =
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
  let input = Bexp.Widget.create_text_input "x" in
  create
    [ widget input (fun { let_rec_ident; _ } -> let_rec_ident)
    ; text "="
    ; nt (fun { let_rec_expr; _ } -> let_rec_expr) expr_data
    ; text "and"
    ; newline
    ; nt (fun { let_rec_next; _ } -> let_rec_next) let_rec_data ]
    ~create:(fun () ->
      { let_rec_ident = Bexp.Widget.create_text_input input#value
      ; let_rec_expr = Bexp.Hole.create get_expr expr_data
      ; let_rec_next = Bexp.Hole.create get_let_rec let_rec_data })
    ~to_term:(fun x -> x)
    ~symbol_of_term:symbol_of_let_rec

let eapp_def =
  let open Bexp.Syntax in
  create [nt left expr_data; text "("; nt right expr_data; text ")"]
    ~create:(fun () ->
      ( Bexp.Hole.create get_expr expr_data
      , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> EApp args)
    ~symbol_of_term:symbol_of_expr

let eassn_def =
  let open Bexp.Syntax in
  create [nt left expr_data; text ":="; nt right expr_data]
    ~create:(fun () ->
      ( Bexp.Hole.create get_expr expr_data
      , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> EAssign args)
    ~symbol_of_term:symbol_of_expr

let ecase_def =
  let open Bexp.Syntax in
  create
    [ text "case"; nt left expr_data; text "of"; newline; nt right branch_data ]
    ~create:(fun () ->
      ( Bexp.Hole.create get_expr expr_data
      , Bexp.Hole.create get_branch branch_data ))
    ~to_term:(fun args -> ECase args)
    ~symbol_of_term:symbol_of_expr

let elam_def =
  let open Bexp.Syntax in
  create [ text "fun"; nt left pat_data; text "->"; nt right expr_data ]
    ~create:(fun () ->
      ( Bexp.Hole.create get_pat pat_data
      , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> ELam args)
    ~symbol_of_term:symbol_of_expr

let elet_def =
  let open Bexp.Syntax in
  create
    [ text "let"; nt left let_def_data; text "in"; newline; nt right expr_data ]
    ~create:(fun () ->
      ( Bexp.Hole.create get_let_def let_def_data
      , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> ELet args)
    ~symbol_of_term:symbol_of_expr

let elet_rec_def =
  let open Bexp.Syntax in
  create
    [ text "let rec"; nt left let_rec_data; text "in"
    ; newline; nt right expr_data ]
    ~create:(fun () ->
      ( Bexp.Hole.create get_let_rec let_rec_data
      , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> ELet_rec args)
    ~symbol_of_term:symbol_of_expr

let eseq_def =
  let open Bexp.Syntax in
  create [ nt left expr_data; text ";"; newline; nt right expr_data ]
    ~create:(fun () ->
      ( Bexp.Hole.create get_expr expr_data
      , Bexp.Hole.create get_expr expr_data ))
    ~to_term:(fun args -> ESeq args)
    ~symbol_of_term:symbol_of_expr

let eunit_def =
  let open Bexp.Syntax in
  create [ text "()" ]
    ~create:(fun () -> ())
    ~to_term:(fun () -> EUnit)
    ~symbol_of_term:symbol_of_expr

let evar_def =
  let open Bexp.Syntax in
  let input = Bexp.Widget.create_text_input "x" in
  create [ widget input id ]
    ~create:(fun () -> Bexp.Widget.create_text_input input#value)
    ~to_term:(fun input -> EVar input)
    ~symbol_of_term:symbol_of_expr

let pconstr_def =
  let open Bexp.Syntax in
  let input = Bexp.Widget.create_text_input "Constr" in
  create [ widget input left; nt right pat_list_data ]
    ~create:(fun () ->
      ( Bexp.Widget.create_text_input input#value
      , Bexp.Hole.create get_pat_list pat_list_data ))
    ~to_term:(fun x -> PConstr x)
    ~symbol_of_term:symbol_of_pat

let pref_def =
  let open Bexp.Syntax in
  create [text "ref"; nt id pat_data ]
    ~create:(fun () -> Bexp.Hole.create get_pat pat_data)
    ~to_term:(fun p -> PRef p)
    ~symbol_of_term:symbol_of_pat

let punit_def =
  let open Bexp.Syntax in
  create [text "()"]
    ~create:(fun () -> ())
    ~to_term:(fun () -> PWild)
    ~symbol_of_term:symbol_of_pat

let pvar_def =
  let open Bexp.Syntax in
  let input = Bexp.Widget.create_text_input "x" in
  create [widget input id]
    ~create:(fun () -> Bexp.Widget.create_text_input input#value)
    ~to_term:(fun input -> PVar input)
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
    ; newline
    ; tab
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
    ; Bexp.Syntax pref_def
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
    ; Bexp.Syntax elet_rec_def
    ; Bexp.Syntax eseq_def
    ; Bexp.Syntax eunit_def
    ; Bexp.Syntax evar_def ]

let let_rec_palette =
  Bexp.Palette.create ctx.Bexp.workspace (Some (Palette expr_palette))
    let_rec_data [ Bexp.Syntax let_rec_def ]

let let_def_palette =
  Bexp.Palette.create ctx.Bexp.workspace (Some (Palette let_rec_palette))
    let_def_data [ Bexp.Syntax let_def_def ]

let items_palette =
  Bexp.Palette.create ctx.Bexp.workspace (Some (Palette let_def_palette))
    items_data [ Bexp.Syntax ilet_def; Bexp.Syntax ilet_rec_def ]

let module_palette =
  Bexp.Palette.create ctx.Bexp.workspace (Some (Palette items_palette))
    module_data [ Bexp.Syntax module_def ]

let rec compile_pattern hole =
  let node = match hole.Bexp.hole_term with
    | None -> Ast.Wild
    | Some term ->
       match term.Bexp.term with
       | PWild -> Ast.Wild
       | PUnit -> Ast.Unit
       | PVar input -> Ast.Var (input#value)
       | PRef pat -> Ast.Deref (compile_pattern pat)
       | PConstr(constr, pats) ->
          Ast.Con(Ast.Internal constr#value, compile_patterns pats)
  in { Ast.pat_ann = Bexp.Hole hole; pat_node = node }

and compile_patterns hole =
  match hole.Bexp.hole_term with
  | None -> []
  | Some { Bexp.term = PList(p, ps); _ }->
     compile_pattern p :: compile_patterns ps

let rec compile_branch hole =
  match hole.Bexp.hole_term with
  | None -> []
  | Some term ->
     let { branch_pat = pat
         ; branch_expr = expr
         ; branch_next = next } = term.Bexp.term
     in (compile_pattern pat, compile_expr expr) :: compile_branch next

and compile_expr hole =
  let node = match hole.Bexp.hole_term with
    | None -> Ast.Typed_hole
    | Some term ->
       match term.Bexp.term with
       | EApp(f, x) -> Ast.App(compile_expr f, compile_expr x)
       | EAssign(l, r) -> Ast.Assign(compile_expr l, compile_expr r)
       | ECase(scrut, branches) ->
          Ast.Case(compile_expr scrut, compile_branch branches)
       | ELam(pat, body) ->
          Ast.Lam((compile_pattern pat, [], compile_expr body), [])
       | ELet(defs, body) ->
          Ast.Let(compile_let_def defs, compile_expr body)
       | ELet_rec(defs, body) ->
          Ast.Let_rec(compile_let_rec defs, compile_expr body)
       | ESeq(f, s) -> Ast.Seq(compile_expr f, compile_expr s)
       | EUnit -> Ast.Lit Literal.Unit
       | EVar input -> Ast.Var (Ast.Internal input#value)
  in { Ast.expr_ann = Bexp.Hole hole; expr_node = node }

and compile_let_def hole =
  match hole.Bexp.hole_term with
  | None -> []
  | Some term ->
     let { let_pat = pat
         ; let_expr = expr
         ; let_next = next } = term.Bexp.term in
     { Ast.let_ann = Bexp.Hole hole
     ; let_lhs = compile_pattern pat
     ; let_rhs = compile_expr expr } :: compile_let_def next

and compile_let_rec hole =
  match hole.Bexp.hole_term with
  | None -> []
  | Some term ->
     let { let_rec_ident = ident
         ; let_rec_expr = expr
         ; let_rec_next = next } = term.Bexp.term in
     { Ast.rec_ann = Bexp.Hole hole
     ; rec_lhs = ident#value
     ; rec_rhs = compile_expr expr } :: compile_let_rec next

let rec compile_items hole =
  match hole.Bexp.hole_term with
  | None -> []
  | Some term ->
     let node, next = match term.Bexp.term with
       | ILet(defs, next) ->
          Ast.Let (compile_let_def defs), next
       | ILet_rec(defs, next) ->
          Ast.Let_rec (compile_let_rec defs), next
     in
     { Ast.item_ann = Bexp.Hole hole; item_node = node } :: compile_items next

let compile_module hole =
  match hole.Bexp.hole_term with
  | None -> None
  | Some modl ->
     Some
       { Ast.file_ann = Bexp.Hole hole
       ; file_exports = []
       ; file_items = compile_items modl.Bexp.term.items }

let typecheck_button =
  match
    Dom_html.getElementById "typecheck"
    |> Dom_html.CoerceTo.button |> Js.Opt.to_option
  with
  | None -> assert false
  | Some button -> button

let () =
  typecheck_button##.onclick :=
    Dom.handler (fun _ ->
        begin match compile_module ctx.Bexp.hole with
        | None -> ()
        | Some modl ->
           match
             let name = "main" in
             let package = Package.create name in
             let packages = Hashtbl.create (module String) in
             let _ = Hashtbl.add packages ~key:name ~data:package in
             let open Result.Monad_infix in
             let typechecker = Typecheck.create package packages in
             let env = Env.empty (module String) in
             Desugar.desugar typechecker env package packages modl
             >>= fun term_file ->
             Typecheck.typecheck typechecker term_file
           with
           | Ok _ -> Caml.print_endline "Ok!"
           | Error _ -> Caml.print_endline "Err!"
        end;
        Js._false
      )

let () =
  Bexp.Toolbox.set_palette ctx.Bexp.workspace.toolbox module_palette;
  ignore (svg##appendChild
            (ctx.Bexp.workspace.root_layer#element :> Dom.node Js.t));
  Bexp.Workspace.render ctx.Bexp.workspace
