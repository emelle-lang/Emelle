# Emelle

Emelle is a work-in-progress
[ML](https://en.wikipedia.org/wiki/ML_(programming_language)) dialect.

## Building

First, download OPAM, the OCaml package manager.

Switch to the latest compiler version (4.07.0 at the time of writing):

    opam switch 4.07.0

Emelle uses the `dune` build tool. To install it, run:

    opam install dune

The library in `src` depends on `base`, `menhir`, and `ppx_jane`:

    opam install base
    opam install menhir
    opam install ppx_jane

To build the library and run the tests, run:

    dune runtest

The executable in `app` additionally requires `stdio`:

    opam install stdio

To build the executable, run:

    dune build app/main.exe

The web interface requires the Js_of_ocaml compiler:

    opam install js_of_ocaml
    opam install js_of_ocaml-ppx

To build it, run:

    dune build try/main.bc.js
    dune build try/stylesheet.css
    dune bulid try/index.html

All of the build output will be in `_build/main`.
