open Base

type qual_id =
  | External of string * string
  | Internal of string

type 'a monotype = {
    ty_ann : 'a;
    ty_node : 'a monotype';
  }

and 'a monotype' =
  | TApp of 'a monotype * 'a monotype
  | TApplied_arrow of 'a monotype * 'a monotype
  | TArrow
  | TRef
  | TNominal of qual_id
  | TVar of string

type typevar_decl =
  | Pure
  | Impure of int

type 'a polytype = {
    polyty_ann : 'a;
    polyty_params : (string * typevar_decl) list;
    polyty_body : 'a monotype;
  }

type 'a pattern = {
    pat_ann : 'a;
    pat_node : 'a pattern';
  }

and 'a pattern' =
  | Con of qual_id * 'a pattern list
  | Deref of 'a pattern
  | Unit
  | Var of string
  | Wild

type 'a expr = {
    expr_ann : 'a;
    expr_node : 'a expr';
  }

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

type 'a datacon = {
    datacon_ann : 'a;
    datacon_name : string;
    datacon_product : 'a monotype list;
  }

type 'a adt = {
    adt_ann : 'a;
    adt_name : string;
    adt_params : string list;
    adt_datacons : 'a datacon list
  }

type 'a item' =
  | Let of ('a pattern * 'a expr) list
  | Let_rec of (string * 'a expr) list
  | Type of 'a adt * 'a adt list

type 'a item = {
    item_ann : 'a;
    item_node : 'a item';
  }

type 'a file = {
    file_ann : 'a;
    file_exports : string list;
    file_items : 'a item list
  }
