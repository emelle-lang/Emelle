open Base
open Stdio
open Emelle

let () =
  let open Result.Monad_infix in
  match
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.expr_eof Lexer.expr lexbuf in
    let package = Package.create "" in
    let env = Env.empty (module String) in
    let packages = Hashtbl.create (module String) in
    let elaborator = Elab.create package packages in
    let typechecker = Typecheck.create package packages in
    Elab.term_of_expr elaborator env ast
    >>= fun term ->
    Typecheck.infer_term typechecker term
    >>| fun typedtree ->
    Typecheck.gen typechecker typedtree.Typedtree.ty;
    typedtree.Typedtree.ty
  with
  | Ok ty ->
     let pprinter = Prettyprint.create () in
     Prettyprint.print_type pprinter (-1) ty;
     print_endline (Prettyprint.to_string pprinter)
  | Error _ -> ()
