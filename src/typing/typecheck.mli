type t = {
    package : Package.t;
    packages : (string, Package.t) Base.Hashtbl.t;
    env : (Ident.t, Type.t) Base.Hashtbl.t;
    let_level : int;
    lam_level : int;
    tvargen : Type.vargen;
    kvargen : Kind.vargen
  }

val create : Package.t -> (string, Package.t) Base.Hashtbl.t -> t

val unify_kinds : Kind.t -> Kind.t -> (unit, Message.error) result

val kind_of_type : t -> Type.t -> (Kind.t, Message.error) result

val gen : t -> Type.t -> unit

val type_adt_of_ast_adt : t -> 'a Ast.adt -> (Type.adt, 'a Message.t) result

val infer_term : t -> 'a Term.t -> ('a Typedtree.t, 'a Message.t) result

val typecheck : t -> 'a Term.file -> ('a Typedtree.file, 'a Message.t) result
