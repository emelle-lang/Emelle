# Typechecking

Emmeline's type system is based on System Fw, a variation of System F with
higher-kinded types. The Emmeline typechecker performs Hindley-Milner type
inference to determine the principal type of a term.

## Terminology

- Monotype: A non-generic type
- Polytype or type scheme: A polymorphic type
- Type variable: A type whose actual value is unknown
- Rigid type variable: A type variable universally quantified over by type
  scheme
- Wobbly type variable: A type variable that represents a fixed unknown

Like type variables, there are also kind variables. Emelle does not feature
polymorphic kinds at the moment, so all unsolved kind pariables default to `*`.

## Algorithm

There are two important operations, generalization (gen) and instantiation
(inst). Generalization universally quantifies over wobbly type variables to
produce the type scheme of a binding. Instantiation replaces rigid type
variables in a type scheme with fresh wobbly ones to give the monotype of a
variable use site.

Emmeline uses level-based generalization like that in the OCaml compiler. When
the typechecker visits the definition of a let-binding, it increments the "let
level." Type variables are associated with the level in which they were
generated.

Type expressions cannot be recursive, so type variables undergo the occurs check
to ensure that they are not in the type that they are being unified with. During
the occurs check, the levels of type variables higher than the variable being
unified are lowered to the same level to avoid premature universal
quantification.

Because Emmeline has mutable references, not all type variables can be
universally quantified.

For example:

    let r = ref None in
    r := Some 0;
    r := Some "foo"

If `r` were generalized as `forall a. Ref (Option a)`, values of different types
could be used together, such as `0` and `"foo"`, making the type system unsound.

Traditionally, MLs have solved this issue with the value restriction, where only
syntactic "value" expressions are generalized. The value restriction hinders
point-free  style.

Emmeline solves the mutable reference issue a different way. Type variables can
either be "pure" or "impure." Type variables are pure until they are "tainted"
by type application with the `Ref` type constructor; then, they become impure.
Type variables also have a "lam level" associated with the outermost lambda
expression that they appear in. Upon function application, all the type
variables in the function's codomain have their levels decremented. If a type
variable is impure and has a lam level of 0, then it is weak.


During the occurs check, Emelle adjust the lambda levels and, if the type
variable being unified is impure, sets the purities of the type variables in the
type expression to impure.

The purity and level of type variables is recorded in the type scheme.

This idea seems to have been previously discovered and used in the SML/NJ
compiler.

See http://okmij.org/ftp/ML/generalization.html for a description of the
level-based typechecking algorithm and
https://core.ac.uk/download/pdf/82104448.pdf for a description of SML/NJ's
lambda-level type variable tracking.
