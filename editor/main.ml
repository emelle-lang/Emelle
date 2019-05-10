(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Core_kernel
open Js_of_ocaml
open Emmeline

type ident =
  | Local of Bexp.Widget.text_input
  | Qual of Bexp.Widget.text_input * Bexp.Widget.text_input

and modul = {
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
  | ERef
  | ESeq of bin_expr
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

and symbols =
  | Ident of (symbols, ident) Bexp.term
  | Module of (symbols, modul) Bexp.term
  | Items of (symbols, items) Bexp.term
  | Let_def of (symbols, let_def) Bexp.term
  | Let_rec of (symbols, let_rec) Bexp.term
  | Expr of (symbols, expr) Bexp.term
  | Pat of (symbols, pat) Bexp.term
  | Pat_list of (symbols, pat_list) Bexp.term
  | Branch of (symbols, branch) Bexp.term

let doc = Dom_svg.document

let get_ident = function
  | Ident i -> Some i
  | _ -> None

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

let symbol_of_ident i = Ident i

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

let ident_data =
  { Bexp.palette_name = "Identifier"
  ; palette_color = "purple" }

let module_data =
  { Bexp.palette_name = "Module"
  ; palette_color = "lime" }

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

let container = Dom_html.getElementById "workspace-span"

let entry_hole = Bexp.Hole.create get_module module_data

let ctx = Bexp.Workspace.create container entry_hole

let id x = x

let local_def =
  let open Bexp.Syntax in
  let input = Bexp.Widget.create_text_input "x" in
  create [ widget input id ]
    ~create:(fun () -> Bexp.Widget.create_text_input input#value)
    ~to_term:(fun s -> Local s)
    ~symbol_of_term:symbol_of_ident

let qual_def =
  let open Bexp.Syntax in
  let input = Bexp.Widget.create_text_input "Prelude" in
  let input2 = Bexp.Widget.create_text_input "x" in
  create [widget input left; text "."; widget input2 right]
    ~create:(fun () ->
      ( Bexp.Widget.create_text_input input#value
      , Bexp.Widget.create_text_input input2#value ))
    ~to_term:(fun (l, r) -> Qual (l, r))
    ~symbol_of_term:symbol_of_ident

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
    [ text "case"; nt left expr_data; text "of"; newline
    ; tab; nt right branch_data ]
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

let eref_def =
  let open Bexp.Syntax in
  create [ text "ref" ]
    ~create:(fun () -> ())
    ~to_term:(fun () -> ERef)
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
  create [ nt id ident_data ]
    ~create:(fun () -> Bexp.Hole.create get_ident ident_data)
    ~to_term:(fun input -> EVar input)
    ~symbol_of_term:symbol_of_expr

let pconstr_def =
  let open Bexp.Syntax in
  create [ nt left ident_data; text "("; nt right pat_list_data; text ")" ]
    ~create:(fun () ->
      ( Bexp.Hole.create get_ident ident_data
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
  create [nt left pat_data; tab; nt right pat_list_data]
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
  Bexp.Palette.create ctx None
    branch_data
    [ Bexp.Syntax branch_def ]

let pat_list_palette =
  Bexp.Palette.create ctx (Some (Palette branch_palette))
    pat_list_data
    [ Bexp.Syntax plist_cons_def ]

let pat_palette =
  Bexp.Palette.create ctx (Some (Palette pat_list_palette))
    pat_data
    [ Bexp.Syntax pconstr_def
    ; Bexp.Syntax punit_def
    ; Bexp.Syntax pref_def
    ; Bexp.Syntax pvar_def
    ; Bexp.Syntax pwild_def ]

let expr_palette =
  Bexp.Palette.create ctx (Some (Palette pat_palette))
    expr_data
    [ Bexp.Syntax eapp_def
    ; Bexp.Syntax eassn_def
    ; Bexp.Syntax ecase_def
    ; Bexp.Syntax elam_def
    ; Bexp.Syntax elet_def
    ; Bexp.Syntax elet_rec_def
    ; Bexp.Syntax eref_def
    ; Bexp.Syntax eseq_def
    ; Bexp.Syntax eunit_def
    ; Bexp.Syntax evar_def ]

let let_rec_palette =
  Bexp.Palette.create ctx (Some (Palette expr_palette))
    let_rec_data [ Bexp.Syntax let_rec_def ]

let let_def_palette =
  Bexp.Palette.create ctx (Some (Palette let_rec_palette))
    let_def_data [ Bexp.Syntax let_def_def ]

let items_palette =
  Bexp.Palette.create ctx (Some (Palette let_def_palette))
    items_data [ Bexp.Syntax ilet_def; Bexp.Syntax ilet_rec_def ]

let module_palette =
  Bexp.Palette.create ctx (Some (Palette items_palette))
    module_data [ Bexp.Syntax module_def ]

let ident_palette =
  Bexp.Palette.create ctx (Some (Palette module_palette))
    ident_data [ Bexp.Syntax local_def; Bexp.Syntax qual_def ]

let compile_ident hole =
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Error (Bexp.Hole hole, "Missing")
  | Some term ->
     match term.Bexp.term with
     | Local input -> Ok (Ast.Internal input#value)
     | Qual(l, r) -> Ok (Ast.External (l#value, r#value))

let rec compile_pattern hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  let%map node = match hole.Bexp.hole_term with
    | None -> Ok Ast.Wild
    | Some term ->
       match term.Bexp.term with
       | PWild -> Ok Ast.Wild
       | PUnit -> Ok Ast.Unit
       | PVar input -> Ok (Ast.Var (input#value))
       | PRef pat ->
          let%map pat = compile_pattern pat in
          Ast.Deref pat
       | PConstr(id, pats) ->
          let%bind id = compile_ident id in
          let%map pats = compile_patterns pats in
          Ast.Con(id, pats)
  in { Ast.pat_ann = Bexp.Hole hole; pat_node = node }

and compile_patterns hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Ok []
  | Some { Bexp.term = PList(p, ps); _ }->
     let%bind p = compile_pattern p in
     let%map ps = compile_patterns ps in
     p::ps

let rec compile_branch hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Ok []
  | Some term ->
     let { branch_pat = pat
         ; branch_expr = expr
         ; branch_next = next } = term.Bexp.term
     in
     let%bind pat = compile_pattern pat in
     let%bind expr = compile_expr expr in
     let%map next = compile_branch next in
     (pat, expr)::next

and compile_expr hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  let%map node = match hole.Bexp.hole_term with
    | None -> Ok Ast.Typed_hole
    | Some term ->
       match term.Bexp.term with
       | EApp(f, x) ->
          let%bind f = compile_expr f in
          let%map x = compile_expr x in
          Ast.App(f, x)
       | EAssign(l, r) ->
          let%bind l = compile_expr l in
          let%map r = compile_expr r in
          Ast.Assign(l, r)
       | ECase(scrut, branches) ->
          let%bind scrut = compile_expr scrut in
          let%map branches = compile_branch branches in
          Ast.Case(scrut, branches)
       | ELam(pat, body) ->
          let%bind pat = compile_pattern pat in
          let%map body = compile_expr body in
          Ast.Lam((pat, [], body), [])
       | ELet(defs, body) ->
          let%bind defs = compile_let_def defs in
          let%map body = compile_expr body in
          (Ast.Let(defs, body) : _ Ast.expr')
       | ELet_rec(defs, body) ->
          let%bind defs = compile_let_rec defs in
          let%map body = compile_expr body in
          (Ast.Let_rec(defs, body) : _ Ast.expr')
       | ERef -> Ok Ast.Ref
       | ESeq(f, s) ->
          let%bind f = compile_expr f in
          let%map s = compile_expr s in
          Ast.Seq(f, s)
       | EUnit -> Ok (Ast.Lit Literal.Unit)
       | EVar ident ->
          let%map ident = compile_ident ident in
          Ast.Var ident
  in { Ast.expr_ann = Bexp.Hole hole; expr_node = node }

and compile_let_def hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Ok []
  | Some term ->
     let { let_pat = pat
         ; let_expr = expr
         ; let_next = next } = term.Bexp.term in
     let%bind pat = compile_pattern pat in
     let%bind expr = compile_expr expr in
     let%map next = compile_let_def next in
     { Ast.let_ann = Bexp.Hole hole; let_lhs = pat; let_rhs = expr } :: next

and compile_let_rec hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Ok []
  | Some term ->
     let { let_rec_ident = ident
         ; let_rec_expr = expr
         ; let_rec_next = next } = term.Bexp.term in
     let%bind expr = compile_expr expr in
     let%map next = compile_let_rec next in
     { Ast.rec_ann = Bexp.Hole hole
     ; rec_lhs = ident#value; rec_rhs = expr } :: next

let rec compile_items hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Ok []
  | Some term ->
     let%bind node, next = match term.Bexp.term with
       | ILet(defs, next) ->
          let%map defs = compile_let_def defs in
          Ast.Let defs, next
       | ILet_rec(defs, next) ->
          let%map defs = compile_let_rec defs in
          Ast.Let_rec defs, next
     in
     let%map next = compile_items next in
     { Ast.item_ann = Bexp.Hole hole; item_node = node }::next

let compile_module hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Error (Bexp.Hole hole, "No module")
  | Some modl ->
     let%map items = compile_items modl.Bexp.term.items in
     { Ast.file_ann = Bexp.Hole hole
     ; file_exports = []
     ; file_items = items }

let typecheck_button =
  match
    Dom_html.getElementById "typecheck"
    |> Dom_html.CoerceTo.button |> Js.Opt.to_option
  with
  | None -> assert false
  | Some button -> button

let console = Dom_html.getElementById "console"

let set_console_text str =
  console##.textContent := Js.some (Js.string str)

let error_message =
  "There are errors in the program! Click the highlighted " ^
  "blocks to see the error message."

let () =
  typecheck_button##.onclick :=
    Dom.handler (fun _ ->
        begin match compile_module entry_hole with
        | Error (Bexp.Hole hole, error) ->
           Bexp.Hole.set_error hole (fun () -> set_console_text error);
           set_console_text error_message
        | Ok modl ->
           match
             let packages = Hashtbl.create (module String) in
             let compiler = Pipeline.create "main" packages in
             let env = Env.empty (module String) in
             Pipeline.compile_frontend compiler env modl
           with
           | Ok _ -> set_console_text ""
           | Error e ->
              let rec f = function
                | Message.And(fst, snd) ->
                   f fst;
                   f snd
                | Message.Diagnostic d ->
                   let Bexp.Hole hole = d.Message.loc in
                   Bexp.Hole.set_error hole
                     (fun () ->
                       let pp = Prettyprint.create () in
                       Prettyprint.print_error pp d.Message.error;
                       set_console_text (Prettyprint.to_string pp))
                | Message.Unreachable str ->
                   Caml.print_endline ("Unreachable " ^ str);
              in
              f e;
              set_console_text error_message
        end;
        Js._false
      )

let () =
  Bexp.Toolbox.set_palette ctx.Bexp.toolbox ident_palette;
  Bexp.Workspace.render ctx
