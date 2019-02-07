(** The compiler must transform statically known curried functions into
    multi-parameter functions, generate constant virtual registers, and compile
    pattern matches into decision trees. *)

open Base

type t = {
    ctx : (Ident.t, Ir.Register.t) Hashtbl.t;
    (** Map from ids to variables *)
    free_vars : (Ir.Register.t * Ir.Operand.t) Queue.t; (** Free variables *)
    pat_ctx : Pattern.context;
    parent : t option;
    reg_gen : Ir.Register.gen;
  }

let create parent =
  let reg_gen = Ir.Register.create_gen () in
  { ctx = Hashtbl.create (module Ident)
  ; free_vars = Queue.create ()
  ; pat_ctx = Pattern.create reg_gen
  ; parent
  ; reg_gen }

let fresh_register self =
  Ir.Register.fresh self.reg_gen

let rec free_var self id =
  (* I would use Hashtbl.find_or_add here, but the callback it takes isn't
     monadic, and my code is able to fail via the result monad. *)
  Hashtbl.find_and_call self.ctx id
    ~if_found:(fun x -> Ok x)
    ~if_not_found:(fun id ->
      let open Result.Monad_infix in
      match self.parent with
      | Some parent ->
         free_var parent id >>= fun var ->
         let reg = fresh_register self in
         let _ = Hashtbl.add self.ctx ~key:id ~data:reg in
         Queue.enqueue self.free_vars (reg, Ir.Operand.Register var);
         Ok reg
      | None ->
         Error (Message.Unreachable_error "Lower free_var")
    )

let make_break self ann instr =
  let reg = fresh_register self in
  { Anf.ann
  ; instr =
      Anf.Let
        ( reg
        , instr
        , { Anf.ann; instr = Anf.Break (Ir.Operand.Register reg) } ) }

(** Combine statically known nested unary functions into multi-argument procs *)
let rec proc_of_typedtree self params ann id body ~cont =
  let reg = fresh_register self in
  match Hashtbl.add self.ctx ~key:id ~data:reg with
  | `Duplicate ->
     Message.error ann (Message.Unreachable_error "Lower uncurry")
  | `Ok ->
     match body.Typedtree.expr with
     | Typedtree.Lam(id, body) ->
        proc_of_typedtree self (reg::params) body.Typedtree.ann id body ~cont
     | _ ->
        instr_of_typedtree self body ~cont:(fun opcode ->
            cont (Anf.Fun { env = Queue.to_list self.free_vars
                          ; params = List.rev (reg::params)
                          ; body = make_break self body.Typedtree.ann opcode
                          ; reg_gen = self.reg_gen }))

(** Combine curried one-argument applications into a function call with all the
    arguments. *)
and flatten_app self count args f x ~cont =
  match f.Typedtree.expr with
  | Typedtree.App(f, x') ->
     operand_of_typedtree self x' ~cont:(fun x' ->
         flatten_app self (count + 1) (x::args) f x' ~cont
       )
  | Typedtree.Constr(tag, fields) ->
     let args = x::args in
     cont (
         if fields = count then
           Anf.Box (tag, args)
         else
           (* Handle a partially applied data constructor by generating a
              closure *)
           let env = List.map ~f:(fun op -> (fresh_register self, op)) args in
           let reg_gen = Ir.Register.create_gen () in
           (* Closure parameters *)
           let params = Ir.Register.gen_regs reg_gen [] (fields - count) in
           (* Operand list of the parameters *)
           let regs_of_params =
             List.map ~f:(fun reg -> Ir.Operand.Register reg) params in
           (* The already known arguments, accessed from within the closure and
              backwards *)
           let free_var_regs =
             List.map ~f:(fun (reg, _) -> Ir.Operand.Register reg) env in
           (* The final operands of the box opcode *)
           let box_contents = List.append free_var_regs regs_of_params in
           let proc =
             { Anf.env
             ; params
             ; body =
                 make_break self f.Typedtree.ann (Anf.Box(tag, box_contents))
             ; reg_gen }
           in Anf.Fun proc
       )
  | Typedtree.Ref ->
     cont (Anf.Ref x)
  | _ ->
     operand_of_typedtree self f ~cont:(fun f ->
         cont (Anf.Call(f, x, args))
       )

(** This function implements the compilation of a case expression, as used in
    [instr_of_lambdacode]. *)
and compile_case self ann scruts matrix ~cont =
  let open Result.Monad_infix in
  let rec loop operands = function
    | scrut::scruts ->
       operand_of_typedtree self scrut ~cont:(fun operand ->
           loop (operand::operands) scruts
         )
    | [] ->
       let scruts = List.rev operands in
       Message.at ann
         (Pattern.decision_tree_of_matrix self.pat_ctx scruts matrix)
       >>= fun tree -> cont tree
  in loop [] scruts

(** Returns a list of parameters (bound variables) of the branch sorted by
    [Ident.t] *)
and compile_branch self bindings =
  let open Result.Monad_infix in
  Set.fold_right ~f:(fun id acc ->
      acc >>= fun params ->
      let param = fresh_register self in
      match Hashtbl.add self.ctx ~key:id ~data:param with
      | `Duplicate ->
         Error (Message.Unreachable_error "Lower compile_branch")
      | `Ok -> Ok (param::params)
    ) ~init:(Ok []) bindings

(** Convert a [Typedtree.t] into an [instr]. *)
and instr_of_typedtree self ({ Typedtree.ann; expr; _ } as typedtree) ~cont =
  let open Result.Monad_infix in
  match expr with
  | Typedtree.App(f, x) ->
     operand_of_typedtree self x ~cont:(fun x ->
         flatten_app self 1 [] f x ~cont
       )
  | Typedtree.Assign(lhs, rhs) ->
     operand_of_typedtree self lhs ~cont:(fun lhs ->
         operand_of_typedtree self rhs ~cont:(fun rhs ->
             cont (Anf.Assign(lhs, rhs))
           )
       )
  | Typedtree.Case(scruts, matrix, branches) ->
     compile_case self ann scruts matrix ~cont:(fun tree ->
         List.fold_right ~f:(fun (bindings, body) acc ->
             acc >>= fun list ->
             Message.at ann (compile_branch self bindings) >>= fun params ->
             instr_of_typedtree self body ~cont:(fun opcode ->
                 Ok (make_break self ann opcode)
               )
             >>| fun body -> (params, body)::list
           ) ~init:(Ok []) branches
         >>= fun branches ->
         cont (Anf.Case(tree, branches))
       )
  | Typedtree.Constr _ | Typedtree.Extern_var _
  | Typedtree.Local_var _ | Typedtree.Lit _ ->
     operand_of_typedtree self typedtree ~cont:(fun operand ->
         cont (Anf.Load operand)
       )
  | Typedtree.Lam(reg, body) ->
     let self = create (Some self) in
     proc_of_typedtree self [] ann reg body ~cont
  | Typedtree.Let(lhs, rhs, body) ->
     instr_of_typedtree self rhs ~cont:(fun rhs ->
         let var = fresh_register self in
         match Hashtbl.add self.ctx ~key:lhs ~data:var with
         | `Duplicate ->
            Message.error ann
              (Message.Unreachable_error "Lower instr_of_lambdacode")
         | `Ok ->
            instr_of_typedtree self body ~cont >>| fun body ->
            { Anf.ann; instr = Anf.Let(var, rhs, body) }
       )
  | Typedtree.Let_rec(bindings, body) ->
     compile_letrec self ann bindings ~cont:(fun bindings ->
         instr_of_typedtree self body ~cont >>| fun body ->
         { Anf.ann; instr = Anf.Let_rec(bindings, body) }
       )
  | Typedtree.Prim op -> cont (Prim op)
  | Typedtree.Ref ->
     let reg_gen = Ir.Register.create_gen () in
     let reg = Ir.Register.fresh reg_gen in
     cont (Anf.Fun
             { env = []
             ; params = [reg]
             ; body = make_break self ann (Anf.Ref (Ir.Operand.Register reg))
             ; reg_gen })
  | Typedtree.Seq(s, t) ->
     instr_of_typedtree self s ~cont:(fun s ->
         instr_of_typedtree self t ~cont >>| fun t ->
         let var = fresh_register self in
         { Anf.ann; instr = Let(var, s, t) }
       )

(** This function implements the compilation of a let-rec expression, as used in
    [instr_of_typedtree]. *)
and compile_letrec self ann bindings ~cont =
  let open Result.Monad_infix in
  List.fold ~f:(fun acc (lhs, rhs) ->
      acc >>= fun list ->
      let var = fresh_register self in
      match Hashtbl.add self.ctx ~key:lhs ~data:var with
      | `Duplicate ->
         Message.error ann (Message.Unreachable_error "Bytecode comp letrec")
      | `Ok -> Ok ((var, rhs)::list)
    ) ~init:(Ok []) bindings >>= fun list ->
  let rec f bindings = function
    | (var, rhs)::rest ->
       instr_of_typedtree self rhs ~cont:(fun opcode ->
           f ((var, opcode)::bindings) rest
         )
    | [] -> cont bindings
  in f [] list

(** [operand_of_typedtree self expr cont] converts [expr] into an
    [operand], passes it to the continuation [cont], and returns an [instr]. *)
and operand_of_typedtree self typedtree ~cont =
  let open Result.Monad_infix in
  match typedtree.Typedtree.expr with
  | Typedtree.Constr(tag, 0) ->
     let var = fresh_register self in
     cont (Ir.Operand.Register var) >>| fun body ->
     { Anf.ann = typedtree.Typedtree.ann
     ; instr = Anf.Let(var, Anf.Box(tag, []), body) }
  | Typedtree.Constr(tag, size) ->
     let reg_gen = Ir.Register.create_gen () in
     let params = Ir.Register.gen_regs reg_gen [] size in
     let vars = List.map ~f:(fun reg -> Ir.Operand.Register reg) params in
     let proc =
       { Anf.env = []
       ; params
       ; body =
           make_break self typedtree.Typedtree.ann (Anf.Box(tag, vars))
       ; reg_gen } in
     let var = fresh_register self in
     cont (Ir.Operand.Register var) >>| fun body ->
     { Anf.ann = typedtree.Typedtree.ann
     ; instr = Anf.Let(var, Anf.Fun proc, body) }
  | Typedtree.Extern_var id -> cont (Ir.Operand.Extern_var id)
  | Typedtree.Lit lit -> cont (Ir.Operand.Lit lit)
  | Typedtree.Local_var id ->
     Message.at typedtree.Typedtree.ann (free_var self id)
     >>= fun reg -> cont (Ir.Operand.Register reg)
  | _ ->
     instr_of_typedtree self typedtree ~cont:(fun rhs ->
         let var = fresh_register self in
         cont (Ir.Operand.Register var) >>| fun body ->
         { Anf.ann = typedtree.Typedtree.ann; instr = Anf.Let(var, rhs, body) }
       )

let compile
      (package : Package.t)
      { Typedtree.top_ann; exports; items; env; typing_ctx; _ } =
  let open Result.Monad_infix in
  let lowerer = create None in
  let rec loop = function
    | { Typedtree.item_ann = ann
      ; item_node = Typedtree.Top_let(scruts, bindings, matrix)}::rest ->
       compile_case lowerer top_ann scruts matrix
         ~cont:(fun tree ->
           Message.at top_ann (compile_branch lowerer bindings)
           >>= fun params ->
           loop rest >>| fun body ->
           make_break lowerer ann (Anf.Case(tree, [params, body]))
         )
    | { Typedtree.item_ann = ann
      ; item_node = Typedtree.Top_let_rec(bindings) }::rest ->
       compile_letrec lowerer top_ann bindings ~cont:(fun bindings ->
           loop rest >>| fun body ->
           { Anf.ann = ann
           ; instr = Anf.Let_rec(bindings, body) }
         )
    | [] ->
       List.fold ~f:(fun acc name ->
           acc >>= fun (i, list) ->
           match Env.find env name with
           | None ->
              Message.error top_ann
                (Message.Unresolved_path (Ast.Internal name))
           | Some id ->
              match Hashtbl.find typing_ctx id with
              | None -> Message.unreachable "Pipeline export 1"
              | Some ty ->
                 match Hashtbl.find lowerer.ctx id with
                 | None -> Message.unreachable "Pipeline export 2"
                 | Some reg ->
                    match Package.add_val package name ty i with
                    | Some () -> Ok (i + 1, (Ir.Operand.Register reg)::list)
                    | None ->
                       Message.error top_ann (Message.Reexported_name name)
         ) ~init:(Ok (0, [])) exports
       >>| fun (_, operands) ->
       make_break lowerer top_ann (Anf.Box (0, List.rev operands)) in
  loop items >>| fun instr ->
  { Anf.top_instr = instr; reg_gen = lowerer.reg_gen }
