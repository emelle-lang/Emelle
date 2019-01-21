(** The compiler must transform statically known curried functions into
    multi-parameter functions, generate constant virtual registers, and compile
    pattern matches into decision tree. *)

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
         Error (Sequence.return (Message.Unreachable "Lower free_var"))
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
let rec proc_of_lambda self params id body ~cont =
  let reg = fresh_register self in
  match Hashtbl.add self.ctx ~key:id ~data:reg with
  | `Duplicate ->
     Error (Sequence.return (Message.Unreachable "Lower uncurry"))
  | `Ok ->
     match body.Lambda.expr with
     | Lambda.Lam(id, body) ->
        proc_of_lambda self (reg::params) id body ~cont
     | _ ->
        instr_of_lambdacode self body ~cont:(fun opcode ->
            cont (Anf.Fun ({ env = Queue.to_list self.free_vars
                           ; params = List.rev (reg::params)
                           ; body = make_break self body.Lambda.ann opcode }))
          )

(** Combine curried one-argument applications into a function call with all the
    arguments. *)
and flatten_app self count args f x ~cont =
  match f.Lambda.expr with
  | Lambda.App(f, x') ->
     operand_of_lambdacode self x' ~cont:(fun x' ->
         flatten_app self (count + 1) (x::args) f x' ~cont
       )
  | Lambda.Constr(tag, fields) ->
     let args = x::args in
     cont (
         if fields = count then
           Anf.Box ((Ir.Operand.Lit (Literal.Int tag))::args)
         else
           (* Handle a partially applied data constructor by generating a
              closure *)
           let env = List.map ~f:(fun op -> (fresh_register self, op)) args in
           (* Closure parameters *)
           let params = Ir.Register.gen_regs [] (fields - count) in
           (* Operand list of the parameters *)
           let regs_of_params =
             List.map ~f:(fun reg -> Ir.Operand.Register reg) params in
           (* The already known arguments, accessed from within the closure and
              backwards *)
           let free_var_regs =
             List.map ~f:(fun (reg, _) -> Ir.Operand.Register reg) env in
           (* The final operands of the box opcode *)
           let box_contents =
             (Ir.Operand.Lit (Literal.Int tag))::
               (List.append free_var_regs regs_of_params) in
           let proc =
             { Anf.env
             ; params
             ; body = make_break self f.Lambda.ann (Anf.Box box_contents) }
           in Anf.Fun proc
       )
  | Lambda.Ref ->
     cont (Anf.Ref x)
  | _ ->
     operand_of_lambdacode self f ~cont:(fun f ->
         cont (Anf.Call(f, x, args))
       )

(** This function implements the compilation of a case expression, as used in
    [instr_of_lambdacode]. *)
and compile_case self scruts matrix ~cont =
  let open Result.Monad_infix in
  let rec loop operands = function
    | scrut::scruts ->
       operand_of_lambdacode self scrut ~cont:(fun operand ->
           loop (operand::operands) scruts
         )
    | [] ->
       let scruts = List.rev operands in
       Pattern.decision_tree_of_matrix self.pat_ctx scruts matrix
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
         Error (Sequence.return
                  (Message.Unreachable "Lower compile_branch"))
      | `Ok -> Ok (param::params)
    ) ~init:(Ok []) bindings

(** Convert a [Lambda.t] into an [instr]. *)
and instr_of_lambdacode self ({ Lambda.ann; expr; _ } as lambda) ~cont =
  let open Result.Monad_infix in
  match expr with
  | Lambda.App(f, x) ->
     operand_of_lambdacode self x ~cont:(fun x ->
         flatten_app self 1 [] f x ~cont
       )
  | Lambda.Assign(lhs, rhs) ->
     operand_of_lambdacode self lhs ~cont:(fun lhs ->
         operand_of_lambdacode self rhs ~cont:(fun rhs ->
             cont (Anf.Assign(lhs, rhs))
           )
       )
  | Lambda.Case(scruts, matrix, branches) ->
     compile_case self scruts matrix ~cont:(fun tree ->
         List.fold_right ~f:(fun (bindings, body) acc ->
             acc >>= fun list ->
             compile_branch self bindings >>= fun params ->
             instr_of_lambdacode self body ~cont:(fun opcode ->
                 Ok (make_break self ann opcode)
               )
             >>| fun body -> (params, body)::list
           ) ~init:(Ok []) branches
         >>= fun branches ->
         cont (Anf.Case(tree, branches))
       )
  | Lambda.Constr _ | Lambda.Extern_var _
  | Lambda.Local_var _ | Lambda.Lit _ ->
     operand_of_lambdacode self lambda ~cont:(fun operand ->
         cont (Anf.Load operand)
       )
  | Lambda.Lam(reg, body) ->
     let self = create (Some self) in
     proc_of_lambda self [] reg body ~cont
  | Lambda.Let(lhs, rhs, body) ->
     instr_of_lambdacode self rhs ~cont:(fun rhs ->
         let var = fresh_register self in
         match Hashtbl.add self.ctx ~key:lhs ~data:var with
         | `Duplicate -> Message.unreachable "Lower instr_of_lambdacode"
         | `Ok ->
            instr_of_lambdacode self body ~cont >>| fun body ->
            { Anf.ann; instr = Anf.Let(var, rhs, body) }
       )
  | Lambda.Let_rec(bindings, body) ->
     compile_letrec self bindings ~cont:(fun bindings ->
         instr_of_lambdacode self body ~cont >>| fun body ->
         { Anf.ann; instr = Anf.Let_rec(bindings, body) }
       )
  | Lambda.Prim op -> cont (Prim op)
  | Lambda.Ref ->
     let reg = Ir.Register.create_gen () |> Ir.Register.fresh in
     cont (Anf.Fun
             { env = []
             ; params = [reg]
             ; body = make_break self ann (Anf.Ref (Ir.Operand.Register reg)) })
  | Lambda.Seq(s, t) ->
     instr_of_lambdacode self s ~cont:(fun s ->
         instr_of_lambdacode self t ~cont >>| fun t ->
         let var = fresh_register self in
         { Anf.ann; instr = Let(var, s, t) }
       )

(** This function implements the compilation of a let-rec expression, as used in
    [instr_of_lambdacode]. *)
and compile_letrec self bindings ~cont =
  let open Result.Monad_infix in
  List.fold ~f:(fun acc (lhs, rhs) ->
      acc >>= fun list ->
      let var = fresh_register self in
      let temp_var = fresh_register self in
      let unused_reg = fresh_register self in
      match Hashtbl.add self.ctx ~key:lhs ~data:var with
      | `Duplicate ->
         Error (Sequence.return (Message.Unreachable "Bytecode comp letrec"))
      | `Ok -> Ok ((var, temp_var, unused_reg, rhs)::list)
    ) ~init:(Ok []) bindings >>= fun list ->
  let rec f bindings = function
    | (var, temp_var, unused, rhs)::rest ->
       instr_of_lambdacode self rhs ~cont:(fun opcode ->
           f ((var, temp_var, unused, opcode)::bindings) rest
         )
    | [] -> cont bindings
  in f [] list

(** [operand_of_lambdacode self lambda cont] converts [lambda] into an
    [operand], passes it to the continuation [cont], and returns an [instr]. *)
and operand_of_lambdacode self lambda ~cont =
  let open Result.Monad_infix in
  match lambda.Lambda.expr with
  | Lambda.Constr(tag, 0) ->
     let var = fresh_register self in
     cont (Ir.Operand.Register var) >>| fun body ->
     { Anf.ann = lambda.Lambda.ann
     ; instr = Anf.Let(var, Anf.Box [Ir.Operand.Lit (Literal.Int tag)], body) }
  | Lambda.Constr(tag, size) ->
     let params = Ir.Register.gen_regs [] size in
     let vars = List.map ~f:(fun reg -> Ir.Operand.Register reg) params in
     let proc =
       { Anf.env = []
       ; params
       ; body =
           make_break self lambda.Lambda.ann
             (Anf.Box((Ir.Operand.Lit (Literal.Int tag))::vars)) } in
     let var = fresh_register self in
     cont (Ir.Operand.Register var) >>| fun body ->
     { Anf.ann = lambda.Lambda.ann; instr = Anf.Let(var, Anf.Fun proc, body) }
  | Lambda.Extern_var id -> cont (Ir.Operand.Extern_var id)
  | Lambda.Lit lit -> cont (Ir.Operand.Lit lit)
  | Lambda.Local_var id ->
     free_var self id >>= fun reg -> cont (Ir.Operand.Register reg)
  | _ ->
     instr_of_lambdacode self lambda ~cont:(fun rhs ->
         let var = fresh_register self in
         cont (Ir.Operand.Register var) >>| fun body ->
         { Anf.ann = lambda.Lambda.ann; instr = Anf.Let(var, rhs, body) }
       )
