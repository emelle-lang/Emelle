%{
%}

%token AND
%token CASE
%token ELSE
%token FORALL
%token FUN
%token IF
%token IN
%token LET
%token REC
%token THEN
%token TYPE
%token WITH

%token LPARENS
%token RPARENS
%token ARROW
%token BAR
%token COLON
%token COLONCOLON
%token COMMA
%token DOT
%token EQUALS
%token STAR
%token UNDERSCORE

%token <string> UIDENT LIDENT

%token EOF

%start <(Lexing.position * Lexing.position) Ast.expr> file
%start <(Lexing.position * Lexing.position) Ast.expr> expr_eof
%start <(Lexing.position * Lexing.position) Ast.monotype> monotype_eof
%start <(Lexing.position * Lexing.position) Ast.adt> adt_eof

%%

file: expr EOF { $1 };
expr_eof: expr EOF { $1 };
monotype_eof: monotype EOF { $1 };
adt_eof: adt EOF { $1 }

upath: UIDENT list(DOT UIDENT { $2 }) {
      match Base.List.rev $2 with
      | [] -> [], $1
      | last::rev_path -> $1::(Base.List.rev rev_path), last
    }

path: list(UIDENT DOT { $1 }) LIDENT { $1, $2 }

adt:
  | UIDENT list(LIDENT) EQUALS option(BAR) separated_list(BAR, constr) {
      Ast.{ name = $1; typeparams = $2; constrs = $5 }
    }
  ;

constr: UIDENT separated_list(STAR, monotype) { ($1, $2) };

polytype: FORALL list(LIDENT) DOT monotype { Ast.Forall($2, $4) };

monotype:
  | monotype_app ARROW monotype {
      let loc = ($symbolstartpos, $endpos) in
      loc, Ast.TApp((loc, Ast.TApp((loc, Ast.TArrow), $1)), $3)
    }
  | monotype_app { $1 }
  ;

monotype_app:
  | monotype_app monotype_atom { ($symbolstartpos, $endpos), Ast.TApp($1, $2) }
  | monotype_atom { $1 }
  ;

monotype_atom:
  | upath { ($symbolstartpos, $endpos), (Ast.TNominal $1) }
  | LIDENT { ($symbolstartpos, $endpos), (Ast.TVar $1) }
  | LPARENS ARROW RPARENS { ($symbolstartpos, $endpos), Ast.TArrow }
  | LPARENS monotype RPARENS { $2 }
  ;

expr:
  | expr_kw { $1 }
  ;

expr_kw:
  | CASE test = expr WITH option(BAR) cases = separated_list(BAR, case) {
        (($symbolstartpos, $endpos), Ast.Case(test, cases))
      }
  | FUN option(BAR) lambda_case list(BAR lambda_case { $2 }) {
        (($symbolstartpos, $endpos), Ast.Lam($3, $4))
      }
  | LET bindings = separated_list(AND, binding) IN body = expr {
        (($symbolstartpos, $endpos), Ast.Let(bindings, body))
      }
  | LET REC bindings = separated_list(AND, rec_binding) IN body = expr {
        (($symbolstartpos, $endpos), Ast.Let_rec(bindings, body))
      }
  | expr_app { $1 }
  ;

case:
  | pattern ARROW expr { ($1, $3) }
  ;

lambda_case:
  | pattern_2 list(pattern_2) ARROW expr { ($1, $2, $4) }
  ;

binding:
  | pattern EQUALS expr { ($1, $3) }
  ;

rec_binding:
  | LIDENT EQUALS expr { ($1, $3) }
  ;

expr_app:
  | expr_app expr_atom { (($symbolstartpos, $endpos), Ast.App($1, $2)) }
  | expr_atom { $1 }
  ;

expr_atom:
  | path { (($symbolstartpos, $endpos), Ast.Var $1) }
  | LPARENS expr RPARENS { $2 }
  ;

pattern:
  | upath nonempty_list(pattern) {
        (($symbolstartpos, $endpos), Ast.Con($1, $2))
      }
  | pattern_2 { $1 }

pattern_2:
  | upath { (($symbolstartpos, $endpos), Ast.Con($1, [])) }
  | LIDENT { (($symbolstartpos, $endpos), Ast.Var $1) }
  | UNDERSCORE { (($symbolstartpos, $endpos), Ast.Wild) }
  | LPARENS pattern RPARENS { $2 }
  ;
