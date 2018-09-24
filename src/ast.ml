open Base

type path = string list * string

type 'a monotype = 'a * 'a monotype'
and 'a monotype' =
  | TApp of 'a monotype * 'a monotype
  | TArrow
  | TFloat
  | TInt
  | TNominal of path
  | TVar of string

type 'a polytype = Forall of string list * 'a monotype

type 'a pattern = 'a * 'a pattern'
and 'a pattern' =
  | Con of path * 'a pattern list
  | Var of string
  | Wild

type 'a expr = 'a * 'a expr'
and 'a expr' =
  | App of 'a expr * 'a expr
  | Case of 'a expr * ('a pattern * 'a expr) list
  | Lam of 'a lambda_case * 'a lambda_case list
  | Let of ('a pattern * 'a expr) list * 'a expr
  | Let_rec of (string * 'a expr) list * 'a expr
  | Var of path
and 'a lambda_case = 'a pattern * 'a pattern list * 'a expr

type 'a adt =
  { name : string
  ; typeparams : string list
  ; constrs : (string * 'a monotype list) list }

type 'a file =
  { adts : 'a adt list list
  ; expr : 'a expr }
