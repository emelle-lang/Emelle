open Base

type t =
  { lowerer : Lower.t
  ; packages : (string, Package.t) Hashtbl.t
  ; package : Package.t }

let create name packages =
  let package = Package.create name in
  let _ = Hashtbl.add packages ~key:name ~data:package in
  { lowerer = Lower.create None
  ; package
  ; packages }

let export self env type_env Ast.{ ann; exports; _ } =
  let open Result.Monad_infix in
  List.fold ~f:(fun acc name ->
      acc >>= fun (i, list) ->
      match Env.find env name with
      | None ->
         Error (Sequence.return (Message.Unresolved_path (Ast.Internal name)))
      | Some id ->
         match Hashtbl.find type_env id with
         | None -> Message.unreachable "Pipeline export 1"
         | Some ty ->
            match Hashtbl.find self.lowerer.Lower.ctx id with
            | None -> Message.unreachable "Pipeline export 2"
            | Some reg ->
               match Package.add_val self.package name ty i with
               | Some () -> Ok (i + 1, (Ir.Operand.Register reg)::list)
               | None -> Error (Sequence.return (Message.Reexported_name name))
    ) ~init:(Ok (0, [])) exports
  >>| fun (_, operands) ->
  Lower.make_break self.lowerer ann (Anf.Box (List.rev operands))

let compile_package self env ast_file =
  let open Result.Monad_infix in
  let typechecker = Typecheck.create self.package self.packages in
  Elab.elab typechecker env self.package self.packages ast_file
  >>= fun term_file ->
  Typecheck.typecheck typechecker term_file
  >>= fun typed_file ->
  let rec loop = function
    | Lambda.Top_let(scruts, bindings, matrix)::rest ->
       Lower.compile_case self.lowerer scruts matrix
         ~cont:(fun tree ->
           Lower.compile_branch self.lowerer bindings >>= fun params ->
           loop rest >>| fun body ->
           Lower.make_break
             self.lowerer typed_file.Lambda.top_ann
             (Anf.Case(tree, [params, body]))
         )
    | Lambda.Top_let_rec(bindings)::rest ->
       Lower.compile_letrec self.lowerer bindings ~cont:(fun bindings ->
           loop rest >>| fun body ->
           { Anf.ann = typed_file.Lambda.top_ann
           ; instr = Anf.Let_rec(bindings, body) }
         )
    | [] -> export self term_file.Term.env typed_file.Lambda.env ast_file
  in loop typed_file.Lambda.items

let compile packages name ast_package =
  let open Result.Monad_infix in
  let st = create name packages in
  compile_package
    st (Env.empty (module String)) ast_package
  >>= Ssa_of_anf.compile_package
  >>= fun package ->
  Liveness.handle_package package
  >>= fun package' ->
  Color.handle_package package'
  >>= fun colorings ->
  To_asm.compile_package colorings package'
  >>| fun _ -> package
