open Base

type qual_id =
  | External of string * string
  | Internal of string

type 'a monotype = 'a * 'a monotype'
and 'a monotype' =
  | TApp of 'a monotype * 'a monotype
  | TArrow
  | TRef
  | TNominal of qual_id
  | TVar of string

type typevar_decl =
  | Pure
  | Impure of int

type 'a polytype = Forall of (string * typevar_decl) list * 'a monotype

type 'a pattern = 'a * 'a pattern'
and 'a pattern' =
  | Con of qual_id * 'a pattern list
  | Deref of 'a pattern
  | Unit
  | Var of string
  | Wild

type 'a expr = 'a * 'a expr'
and 'a expr' =
  | App of 'a expr * 'a expr
  | Assign of 'a expr * 'a expr
  | Case of 'a expr * ('a pattern * 'a expr) list
  | Constr of qual_id
  | Lam of 'a lambda_case * 'a lambda_case list
  | Let of ('a pattern * 'a expr) list * 'a expr
  | Let_rec of (string * 'a expr) list * 'a expr
  | Lit of Literal.t
  | Prim of string * 'a polytype
  | Ref
  | Seq of 'a expr * 'a expr
  | Var of qual_id
and 'a lambda_case = 'a pattern * 'a pattern list * 'a expr

type 'a adt = {
    name : string;
    typeparams : string list;
    constrs : (string * 'a monotype list) list
  }

type 'a item =
  | Let of ('a pattern * 'a expr) list
  | Let_rec of (string * 'a expr) list
  | Type of 'a adt * 'a adt list

type 'a file = {
    ann : 'a;
    exports : string list;
    items : 'a item list
  }
