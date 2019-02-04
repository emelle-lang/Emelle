%{
%}

%token AND
%token CASE
%token ELSE
%token FORALL
%token FOREIGN
%token FUN
%token IF
%token IN
%token LET
%token REC
%token REF
%token SELF
%token THEN
%token TYPE
%token WITH

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

let package :=
  | LPARENS;
      exports = separated_list(COMMA, LIDENT);
    RPARENS;
    items = list(item);
      { { Ast.ann = ($symbolstartpos, $endpos)
        ; items = items
        ; exports = exports } }

let item :=
  | LET; bindings = separated_list(AND, binding); { Ast.Let bindings }
  | LET; REC; bindings = separated_nonempty_list(AND, rec_binding);
      { Ast.Let_rec bindings }
  | TYPE; ~ = adt; adts = list(AND; adt); { Ast.Type(adt, adts) }

let adt :=
  | name = UIDENT;
    params = list(LIDENT);
    EQUALS; option(BAR);
    constrs = separated_list(BAR, constr);
      { Ast.{ name = name; typeparams = params; constrs = constrs } }

let constr :=
  | name = UIDENT; tys = separated_list(STAR, monotype); { (name, tys) }

let tvar_decl :=
  | id = LIDENT; opt = option(BANG; i = INT_LIT; { i });
      { let purity =
          match opt with
          | None -> Ast.Pure
          | Some i -> Ast.Impure i
        in (id, purity) }

let polytype :=
  | FORALL; tvars = list(tvar_decl); DOT; ty = monotype;
      { Ast.Forall(tvars, ty) }

let monotype :=
  | dom = monotype_app; ARROW; codom = monotype;
      { let loc = ($symbolstartpos, $endpos) in
        loc, Ast.TApp((loc, Ast.TApp((loc, Ast.TArrow), dom)), codom) }
  | monotype_app

let monotype_app :=
  | dom = monotype_app; codom = monotype_atom;
      { ($symbolstartpos, $endpos), Ast.TApp(dom, codom) }
  | monotype_atom

let monotype_atom :=
  | x = qual_uid; { ($symbolstartpos, $endpos), (Ast.TNominal x) }
  | x = LIDENT; { ($symbolstartpos, $endpos), (Ast.TVar x) }
  | LPARENS; ARROW; RPARENS; { ($symbolstartpos, $endpos), Ast.TArrow }
  | REF; { ($symbolstartpos, $endpos), Ast.TRef }
  | LPARENS; ~ = monotype; RPARENS; { monotype }

let expr := expr_kw

let expr_kw :=
  | CASE; test = expr; WITH; option(BAR); cases = separated_list(BAR, case);
      { (($symbolstartpos, $endpos), Ast.Case(test, cases)) }
  | FOREIGN; x = STRING_LIT; y = polytype;
      { (($symbolstartpos, $endpos), Ast.Prim(x, y)) }
  | FUN; option(BAR); case = lambda_case; cases = list(BAR; lambda_case);
      { (($symbolstartpos, $endpos), Ast.Lam(case, cases)) }
  | LET; bindings = separated_list(AND, binding); IN; body = expr;
      { (($symbolstartpos, $endpos), Ast.Let(bindings, body)) }
  | LET; REC; bindings = separated_list(AND, rec_binding); IN; body = expr;
      { (($symbolstartpos, $endpos), Ast.Let_rec(bindings, body)) }
  | expr_seq

let case :=
  | p = pattern; ARROW; e = expr; { (p, e) }

let lambda_case :=
  | p = pattern_2; ps = list(pattern_2); ARROW; e = expr; { (p, ps, e) }

let binding :=
  | p = pattern; EQUALS; e = expr; { (p, e) }

let rec_binding :=
  | id = LIDENT; EQUALS; e = expr; { (id, e) }

let expr_seq :=
  | s = expr_assn; SEMICOLON; t = expr;
      { (($symbolstartpos, $endpos), Ast.Seq(s, t)) }
  | expr_assn

let expr_assn :=
  | lval = expr_app; COLONEQUALS; rval = expr_assn;
      { (($symbolstartpos, $endpos), Ast.Assign(lval, rval)) }
  | expr_app

let expr_app :=
  | f = expr_app; x = expr_atom; { (($symbolstartpos, $endpos), Ast.App(f, x)) }
  | expr_atom

let expr_atom :=
  | ~ = qual_lid; { (($symbolstartpos, $endpos), Ast.Var qual_lid) }
  | ~ = qual_uid; { (($symbolstartpos, $endpos), Ast.Constr qual_uid) }
  | f = FLOAT_LIT; { (($symbolstartpos, $endpos), Ast.Lit (Literal.Float f)) }
  | i = INT_LIT; { (($symbolstartpos, $endpos), Ast.Lit (Literal.Int i)) }
  | s = STRING_LIT; { (($symbolstartpos, $endpos), Ast.Lit (Literal.String s)) }
  | REF; { (($symbolstartpos, $endpos), Ast.Ref) }
  | LPARENS; RPARENS; { (($symbolstartpos, $endpos), Ast.Lit Literal.Unit) }
  | LPARENS; e = expr; RPARENS; { e }

let pattern :=
  | constr = qual_uid; pats = nonempty_list(pattern_2);
      { (($symbolstartpos, $endpos), Ast.Con(constr, pats)) }
  | REF; pat = pattern_2; { (($symbolstartpos, $endpos), Ast.Deref pat) }
  | pattern_2

let pattern_2 :=
  | ~ = qual_uid; { (($symbolstartpos, $endpos), Ast.Con(qual_uid, [])) }
  | id = LIDENT; { (($symbolstartpos, $endpos), Ast.Var id) }
  | UNDERSCORE; { (($symbolstartpos, $endpos), Ast.Wild) }
  | LPARENS; RPARENS; { (($symbolstartpos, $endpos), Ast.Unit) }
  | LPARENS; pat = pattern; RPARENS; { pat }
