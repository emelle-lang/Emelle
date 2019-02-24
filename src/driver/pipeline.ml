open Base

type t =
  { packages : (string, Package.t) Hashtbl.t
  ; package : Package.t }

let create name packages =
  let package = Package.create name in
  let _ = Hashtbl.add packages ~key:name ~data:package in
  { package
  ; packages }

let compile_frontend self env ast_file =
  let open Result.Monad_infix in
  let typechecker = Typecheck.create self.package self.packages in
  Desugar.desugar typechecker env self.package self.packages ast_file
  >>= fun term_file ->
  Typecheck.typecheck typechecker term_file
  >>= fun typed_file ->
  Lower.compile self.package typed_file

let compile packages name ast_package =
  let open Result.Monad_infix in
  let st = create name packages in
  compile_frontend st (Env.empty (module String)) ast_package
  >>= Ssa_of_anf.compile_file
  >>= fun package ->
  Liveness.handle_file package
  >>= fun package' ->
  Color.handle_file package'
  >>= fun colorings ->
  To_asm.compile colorings package'
  >>| fun package' -> (package, package')
