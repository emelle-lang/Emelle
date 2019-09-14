open Base
open Emmeline
open Blockspec

let compile_ident hole =
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Error (Bexp.Hole hole, "Missing")
  | Some term ->
     match term.Bexp.term with
     | Local input -> Ok (Ast.Internal input#value)
     | Qual(l, r) -> Ok (Ast.External (l#value, r#value))

let rec compile_ty hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Error (Bexp.Hole hole, "Missing")
  | Some term ->
     let%map node =match term.Bexp.term with
       | TApp(f, x) ->
          let%bind f = compile_ty f in
          let%map x = compile_ty x in
          Ast.TApp(f, x)
       | TArr -> Ok Ast.TArrow
       | TNominal id ->
          let%map id = compile_ident id in
          Ast.TNominal id
       | TRef -> Ok Ast.TRef
       | TVar input -> Ok (Ast.TVar input#value)
     in { Ast.ty_ann = Bexp.Hole hole; ty_node = node }

let rec compile_product_ty hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Ok []
  | Some term ->
     let%bind factor = compile_ty term.Bexp.term.prod_factor in
     let%map next = compile_product_ty term.Bexp.term.prod_next in
     factor :: next

let rec compile_constrs hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Ok []
  | Some term ->
     let%bind ty = compile_product_ty term.Bexp.term.constr_ty in
     let%map next = compile_constrs term.Bexp.term.constr_next in
     { Ast.datacon_ann = Bexp.Hole hole
     ; datacon_name = term.Bexp.term.constr_name#value
     ; datacon_product = ty } :: next

let rec compile_typeparam hole =
  match hole.Bexp.hole_term with
  | None -> []
  | Some typeparam ->
     let { typeparam_name; typeparam_next } = typeparam.Bexp.term in
     let next = compile_typeparam typeparam_next in
     typeparam_name#value :: next

let rec compile_typedef hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Ok []
  | Some typedef ->
     match typedef.Bexp.term with
     | Adt(name, typeparams, def, next) ->
        let typeparams = compile_typeparam typeparams in
        let%bind def = compile_constrs def in
        let%map next = compile_typedef next in
        { Ast.adt_ann = Bexp.Hole hole
        ; adt_name = name#value
        ; adt_params = typeparams
        ; adt_datacons = def } :: next

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
     p :: ps

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
     (pat, expr) :: next

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
       | EChar input ->
          if String.length input#value = 1 then
            Ok (Ast.Lit (Literal.Char (String.get input#value 0)))
          else
            Error (Bexp.Hole hole, "Input must be a single character")
       | EConstr name ->
          let%map name = compile_ident name in
          Ast.Constr name
       | EFloat input ->
          (try
             let float = Float.of_string input#value in
             Ok (Ast.Lit (Literal.Float float))
           with
           | _ -> Error (Bexp.Hole hole, "Invalid decimal"))
       | EInt input ->
          (try
             let int = Int.of_string input#value in
             Ok (Ast.Lit (Literal.Int int))
           with
           | _ -> Error (Bexp.Hole hole, "Invalid integer"))
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
       | EOp(lhs, op, rhs) ->
          let%bind lhs = compile_expr lhs in
          let%bind rhs = compile_expr rhs in
          let%map op = compile_ident op in
          Ast.Op(lhs, op, rhs)
       | ERef -> Ok Ast.Ref
       | ESeq(f, s) ->
          let%bind f = compile_expr f in
          let%map s = compile_expr s in
          Ast.Seq(f, s)
       | EString input ->
          Ok (Ast.Lit (Literal.String input#value))
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

let rec compile_imports hole =
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | Some term ->
     let { import_package; import_path; import_alias; import_next } =
       term.Bexp.term
     in
     { Ast.import_ann = Bexp.Hole hole
     ; import_package = import_package#value
     ; import_path = [import_path#value]
     ; import_alias = Some import_alias#value } :: compile_imports import_next
  | None -> []

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
       | ITypedefs(defs, next) ->
          let%bind defs = compile_typedef defs in
          match defs with
          | [] -> Error (Bexp.Hole hole, "No types")
          | def :: defs ->
             Ok ( Ast.Type(Ast.Adt def, List.map ~f:(fun t -> Ast.Adt t) defs)
                , next )
     in
     let%map next = compile_items next in
     { Ast.item_ann = Bexp.Hole hole; item_node = node } :: next

let compile_module hole =
  let open Result.Let_syntax in
  Bexp.Hole.clear_error hole;
  match hole.Bexp.hole_term with
  | None -> Error (Bexp.Hole hole, "No module")
  | Some term ->
     let%map items = compile_items term.Bexp.term.items in
     { Ast.file_ann = Bexp.Hole hole
     ; file_imports = compile_imports term.Bexp.term.imports
     ; file_exports = []
     ; file_items = items }
