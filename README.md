# Emmeline

Emmeline is a work-in-progress
[ML](https://en.wikipedia.org/wiki/ML_(programming_language)) dialect.

**See the block-based editor at
https://emmeline.gitlab.io/emmeline/editor/index.html.**

## Building

First, download OPAM, the OCaml package manager. Then, run:

    opam install . --deps-only
    opam install ./bexp --deps-only
    dune runtest
    dune build app/main.exe
    dune build try/main.bc.js
    dune build try/index.html
    dune build try/stylesheet.css
    dune build editor/main.bc.js
    dune build editor/index.html
    dune build editor/stylesheet.css

All of the build output will be in `_build/default`.

## Running the text-based interpreter

To run the interpreter, run `dune exec app/main.exe`. The interpreter will read
an Emmeline program from standard input. To run the contents of a file, pipe its
contents into the interpreter:

    cat myfile.ml | dune exec app/main.exe

Example programs are in the `examples/` directory.
