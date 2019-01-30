# Hacking on the Emelle compiler

The compiler is separated into:
- The Emelle library, located in `src/`
- The Emelle executable, located in `app/`
- The web-based interface, located in `try/`

The Emelle library does not depend on Stdio, so it may be compiled to
JavaScript.

## Compiler design

The compiler phases are as follows, divided into subdirectories:

- `src/syntax`: Lexes and parses the source text into an AST
- `src/desugar`: Elaborates the AST into a core form for the typechecker
- `src/typing`: Performs typechecking and kindchecking, emitting a typed tree
- `src/anf`: Performs A-normalization into Administrative Normal Form (ANF),
  where each intermediate expression is let-bound, performs pattern-match
  compilation into a decision tree, and computes explicit closure capture
- `src/ssa`: Tranforms the ANF into static single assignment (SSA) form, where
  each function consists of a set of basic blocks with branching at the end,
  but not in the middle
- `src/regalloc`: Maps SSA virtual registers to stack offsets, translates basic
  block parameters into actual moves

- `src/common`: Contains modules not tied to any particular phase
- `src/driver`: Contains the code that ties all the phases together
