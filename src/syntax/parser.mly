%{
%}

%token AND
%token AS
%token CASE
%token EXPORT
%token FORALL
%token FOREIGN
%token FUN
%token IMPORT
%token IN
%token LET
%token REC
%token REF
%token TYPE
%token WITH

%token LBRACE
%token RBRACE
%token LPARENS
%token RPARENS
%token ARROW
%token BANG
%token BAR
%token COLON
%token COLONCOLON
%token COLONEQUALS
%token COMMA
%token DOT
%token EQUALS
%token QUESTION
%token SEMICOLON
%token STAR
%token UNDERSCORE

%token <string> UIDENT LIDENT
%token <float> FLOAT_LIT
%token <int> INT_LIT
%token <string> STRING_LIT

%token EOF

%start <(Lexing.position * Lexing.position) Ast.file> file
%start <(Lexing.position * Lexing.position) Ast.expr> expr_eof
%start <(Lexing.position * Lexing.position) Ast.monotype> monotype_eof
%start <(Lexing.position * Lexing.position) Ast.adt> adt_eof

%%

let file :=
  | ~ = package; EOF; { package }

let expr_eof :=
  | ~ = expr; EOF; { expr }

let monotype_eof :=
  | ~ = monotype; EOF; { monotype }

let adt_eof :=
  | ~ = adt; EOF; { adt }

let qual_uid :=
  | x = UIDENT; DOT; y = UIDENT; { Ast.External(x, y) }
  | x = UIDENT; { Ast.Internal x }

let qual_lid :=
  | x = UIDENT; DOT; y = LIDENT; { Ast.External(x, y) }
  | x = LIDENT; { Ast.Internal x }

let exports :=
  | EXPORT; LPARENS; l = separated_list(COMMA, LIDENT); RPARENS; { l }

let import :=
  | IMPORT;
    import_package = STRING_LIT;
    import_path = separated_nonempty_list(DOT, UIDENT);
    import_alias = option(AS; id = UIDENT; { id } );
      { { Ast.import_ann = ($symbolstartpos, $endpos)
        ; import_package
        ; import_path
        ; import_alias } }

let package :=
  | exports = option(exports);
    imports = list(import);
    items = list(item);
      { { Ast.file_ann = ($symbolstartpos, $endpos)
        ; file_imports = imports
        ; file_items = items
        ; file_exports =
            match exports with
            | Some exports -> exports
            | None -> [] } }

let item :=
  | LET; bindings = separated_list(AND, binding);
      { { Ast.item_ann = ($symbolstartpos, $endpos)
        ; item_node = Ast.Let bindings } }
  | LET; REC; bindings = separated_nonempty_list(AND, rec_binding);
      { { Ast.item_ann = ($symbolstartpos, $endpos)
        ; item_node = Ast.Let_rec bindings } }
  | TYPE; ~ = typedecl; typedecls = list(AND; typedecl);
      { { Ast.item_ann = ($symbolstartpos, $endpos)
        ; item_node = Ast.Type(typedecl, typedecls) } }

let typedecl :=
  | ~ = adt; { Ast.Adt adt }
  | ~ = record; { Ast.Record record }

let adt :=
  | name = UIDENT; params = list(LIDENT); EQUALS; option(BAR);
    constrs = separated_list(BAR, constr);
      { { Ast.adt_ann = ($symbolstartpos, $endpos)
        ; adt_name = name
        ; adt_params = params
        ; adt_datacons = constrs } }

let constr :=
  | name = UIDENT; tys = separated_list(STAR, monotype);
      { { Ast.datacon_ann = ($symbolstartpos, $endpos)
        ; datacon_name = name
        ; datacon_product = tys } }

let record :=
  | name = UIDENT; params = list(LIDENT); EQUALS; LBRACE; ~ = fields; RBRACE;
      { { Ast.record_ann = ($symbolstartpos, $endpos)
        ; record_name = name
        ; record_params = params
        ; record_fields = fields } }

let fields :=
  | ~ = field; { [field] }
  | ~ = field; SEMICOLON; { [field] }
  | ~ = field; SEMICOLON; ~ = fields; { field :: fields }

let field :=
  | name = LIDENT; COLON; ~ = polytype;
      { { Ast.field_ann = ($symbolstartpos, $endpos)
        ; field_name = name
        ; field_polytype = polytype } }

let tvar_decl :=
  | id = LIDENT; opt = option(BANG; i = INT_LIT; { i });
      { let purity =
          match opt with
          | None -> Ast.Pure
          | Some i -> Ast.Impure i
        in (id, purity) }

let polytype :=
  | FORALL; tvars = list(tvar_decl); DOT; ty = monotype;
      { { Ast.polyty_ann = ($symbolstartpos, $endpos)
        ; polyty_params = tvars
        ; polyty_body = ty } }

let monotype :=
  | dom = monotype_app; ARROW; codom = monotype;
      { let loc = ($symbolstartpos, $endpos) in
        { Ast.ty_ann = loc
        ; ty_node = Ast.TApplied_arrow(dom, codom) } }
  | monotype_app

let monotype_app :=
  | dom = monotype_app; codom = monotype_atom;
      { { Ast.ty_ann = ($symbolstartpos, $endpos)
        ; ty_node = Ast.TApp(dom, codom) } }
  | monotype_atom

let monotype_atom :=
  | x = qual_uid;
      { { Ast.ty_ann = ($symbolstartpos, $endpos); ty_node = Ast.TNominal x } }
  | x = LIDENT;
      { { Ast.ty_ann = ($symbolstartpos, $endpos); ty_node = (Ast.TVar x) } }
  | LPARENS; ARROW; RPARENS;
      { { Ast.ty_ann = ($symbolstartpos, $endpos); ty_node = Ast.TArrow } }
  | REF; { { Ast.ty_ann = ($symbolstartpos, $endpos); ty_node =  Ast.TRef } }
  | LPARENS; ~ = monotype; RPARENS; { monotype }

let expr := expr_kw

let expr_kw :=
  | CASE; test = expr; WITH; option(BAR); cases = separated_list(BAR, case);
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node =  Ast.Case(test, cases) } }
  | FOREIGN; x = STRING_LIT; y = polytype;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Prim(x, y) } }
  | FUN; option(BAR); case = lambda_case; cases = list(BAR; lambda_case);
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Lam(case, cases) } }
  | LET; bindings = separated_list(AND, binding); IN; body = expr;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node =  Ast.Let(bindings, body) } }
  | LET; REC; bindings = separated_list(AND, rec_binding); IN; body = expr;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Let_rec(bindings, body) } }
  | expr_seq

let case :=
  | p = pattern; ARROW; e = expr; { (p, e) }

let lambda_case :=
  | p = pattern2; ps = list(pattern2); ARROW; e = expr; { (p, ps, e) }

let binding :=
  | p = pattern; EQUALS; e = expr;
      { { Ast.let_ann = ($symbolstartpos, $endpos)
        ; let_lhs = p
        ; let_rhs = e } }

let rec_binding :=
  | id = LIDENT; EQUALS; e = expr;
      { { Ast.rec_ann = ($symbolstartpos, $endpos)
        ; rec_lhs = id
        ; rec_rhs = e } }

let expr_seq :=
  | s = expr_assn; SEMICOLON; t = expr;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Seq(s, t)} }
  | expr_assn

let expr_assn :=
  | lval = expr_app; COLONEQUALS; rval = expr_assn;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Assign(lval, rval) } }
  | expr_app

let expr_app :=
  | f = expr_app; x = expr_atom;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.App(f, x) } }
  | expr_atom

let expr_atom :=
  | ~ = qual_lid;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Var qual_lid } }
  | ~ = qual_uid;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Constr qual_uid } }
  | ~ = expr_atom; DOT; ~ = qual_lid;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Field_access(expr_atom, qual_lid) } }
  | f = FLOAT_LIT;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Lit (Literal.Float f) } }
  | i = INT_LIT;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Lit (Literal.Int i) } }
  | s = STRING_LIT;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Lit (Literal.String s)} }
  | REF; { { Ast.expr_ann = ($symbolstartpos, $endpos); expr_node =  Ast.Ref } }
  | LBRACE; ~ = expr_fields; RBRACE;
      { let field, fields = expr_fields in
        { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Record(field, fields) } }
  | LPARENS; RPARENS;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node =  Ast.Lit Literal.Unit } }
  | QUESTION;
      { { Ast.expr_ann = ($symbolstartpos, $endpos)
        ; expr_node = Ast.Typed_hole } }
  | LPARENS; ~ = expr; RPARENS; { expr }

let expr_fields :=
  | field = expr_field; { (field, []) }
  | field = expr_field; SEMICOLON; { (field, []) }
  | field = expr_field; SEMICOLON; ~ = expr_fields;
      { let field1, fields = expr_fields in (field, field1 :: fields) }

let expr_field :=
  | name = LIDENT; EQUALS; expr = expr_assn; { (name, expr) }

let pattern :=
  | constr = qual_uid; pats = nonempty_list(pattern2);
      { { Ast.pat_ann = ($symbolstartpos, $endpos)
        ; pat_node = Ast.Con(constr, pats) } }
  | REF; pat = pattern2;
      { { Ast.pat_ann = ($symbolstartpos, $endpos)
        ; pat_node = Ast.Deref pat } }
  | pattern2

let pattern2 :=
  | ~ = qual_uid;
      { { Ast.pat_ann = ($symbolstartpos, $endpos)
        ; pat_node = Ast.Con(qual_uid, []) } }
  | id = LIDENT;
      { { Ast.pat_ann = ($symbolstartpos, $endpos); pat_node = Ast.Var id } }
  | UNDERSCORE;
      { { Ast.pat_ann = ($symbolstartpos, $endpos); pat_node = Ast.Wild } }
  | LPARENS; RPARENS;
      { { Ast.pat_ann = ($symbolstartpos, $endpos); pat_node = Ast.Unit } }
  | LPARENS; pat = pattern; RPARENS; { pat }
