# Typechecking

Emelle's type system is based on System Fw, a variation of System F with higher-
kinded types. The Emelle typechecker performs Hindley-Milner type inference to
determine the principle type of a term.

## Terminology

- Monotype: A non-generic type
- Polytype or type scheme: A polymorphic type
- Type variable: A type whose actual value is unknown
- Universally quantified ("forall") type variable: A type variable bound in a
  type scheme, which is replaced with a fresh type variable when the type is
  instantiated
- Existentially quantified type variable: A type variable that represents a
  fixed unknown

Like type variables, there are also kind variables. Emelle does not feature
polymorphic kinds at the moment, so all kind variables are existential.

## Algorithm

There are two important operations, generalization (gen) and instantiation
(inst). Generalization universally quantifies over existential type variables to
produce the type scheme of a binding. Instantiation replaces universal type
variables in a type scheme with fresh existential type variables to give the
monotype of a variable use site.

Emelle uses level-based generalization like that in the OCaml compiler. When the
typechecker visits the RHS of a let binding, it increments the "level." Type
variables are associated with the level in which they were generated.

Type expressions cannot be recursive, so type variables undergo the occurs check
to ensure that they are not in the type that they are being unified with. During
the occurs check, the levels of type variables higher than the variable being
unified are lowered to the same level to avoid premature universal
quantification.

Because Emelle has mutable references, not all type variables can be universally
quantified.

For example:

    let r = ref None in
    r := Some 0;
    r := Some "foo"

If `r` were generalized as `forall a. Option a`, values of different types could
be used together, such as `0` and `"foo"`, making the type system unsound.

Traditionally, MLs have solved this issue with the value restriction, where only
syntactic "value" expressions are generalized. The value restriction hinders
functional style (such as point-free).

Emelle solves the mutable reference issue with a solution similar to OCaml's
level-based generalization. The key is that type variables totally enclosed
within a lambda expression can be safely generalized whether they are part
of the type of mutable bindings or not, as the bindings bound by a lambda
expression do not represent actual memory locations, but rather get instantiated
with fresh memory allocations upon function application.

The type of `fun x -> ref x` is `forall a!1 . a -> Ref a`. The function is a
syntactic value, so if it is the RHS of a let binding, the binding would get
generalized under the value restriction. However, the type variable should also
be generalized even when the function is part of some larger expression.

    let id = fun x -> x in
    let generalize_me = id (fun x -> ref x) in
    ...

In addition to tracking let levels, the Emelle typechecker also tracks lambda
levels and knows that type variables contained within a lambda expression are
safe to generalize.

In addition, Emelle differentiates between "pure" and "impure" type variables.
Type variables in types of values held in a mutable reference are impure; all
others are pure. The lambda expression restriction only applies to impure type
variables.

During the occurs check, Emelle adjust the lambda levels and, if the type
variable being unified is impure, sets the purities of the type variables in the
type expression to impure.

Upon function application, the levels of the type variables in the function's
type that are higher than the current level (meaning that they are bound by the
function) are decremented.

The purity and level of type variables is recorded in the type scheme.

This idea seems to have been previously discovered and used in the SML/NJ
compiler.

See http://okmij.org/ftp/ML/generalization.html for a description of the
level-based typechecking algorithm and
https://core.ac.uk/download/pdf/82104448.pdf for a description of SML/NJ's
lambda-level type variable tracking.
