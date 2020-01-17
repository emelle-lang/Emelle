(* Copyright (C) 2018-2020 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

type t =
  { buffer : Buffer.t
  ; indentation : int }

let create () =
  { buffer = Buffer.create 12
  ; indentation = 0 }

let to_string pp = Buffer.contents pp.buffer

let indent pp f =
  f { pp with indentation = pp.indentation + 1 }

let newline pp =
  Buffer.add_char pp.buffer '\n';
  for _ = 1 to pp.indentation do
    Buffer.add_string pp.buffer "  "
  done

let print_prefix pp { Qual_id.Prefix.package; path } =
  Buffer.add_string pp.buffer package;
  Buffer.add_string pp.buffer "::";
  List.iter path ~f:(fun part ->
      Buffer.add_string pp.buffer part;
      Buffer.add_char pp.buffer '.'
    )

let print_ident pp { Qual_id.prefix; name } =
  print_prefix pp prefix;
  Buffer.add_string pp.buffer name

let print_path pp = function
  | Ast.Internal str ->
     Buffer.add_string pp.buffer str
  | Ast.External(l, r) ->
     Buffer.add_string pp.buffer l;
     Buffer.add_string pp.buffer ".";
     Buffer.add_string pp.buffer r

let with_necessary_parens f pp parent_prec prec =
  if parent_prec >= prec then (
    Buffer.add_char pp.buffer '(';
    f pp;
    Buffer.add_char pp.buffer ')';
  ) else
    f pp

let print_wobbly pp wobbly =
  Buffer.add_char pp.buffer '_';
  Buffer.add_char pp.buffer 't';
  Buffer.add_string pp.buffer (Int.to_string wobbly.Type.wobbly_id)

let print_rigid pp rigid =
  Buffer.add_char pp.buffer 't';
  Buffer.add_string pp.buffer (Int.to_string rigid.Type.rigid_id)

let rec print_type pp parent_prec ty =
  match ty with
  | Type.App(f, x) ->
     let prec = 1 in
     with_necessary_parens (fun pp ->
         print_type pp (prec - 1) f;
         Buffer.add_char pp.buffer ' ';
         print_type pp prec x
       ) pp parent_prec prec
  | Type.Nominal id ->
     print_ident pp id
  | Type.Prim Type.Arrow ->
     Buffer.add_string pp.buffer "(->)"
  | Type.Prim Type.Char ->
     Buffer.add_string pp.buffer "Char"
  | Type.Prim Type.Int ->
     Buffer.add_string pp.buffer "Int"
  | Type.Prim Type.Float ->
     Buffer.add_string pp.buffer "Float"
  | Type.Prim Type.Ref ->
     Buffer.add_string pp.buffer "Ref"
  | Type.Prim Type.String ->
     Buffer.add_string pp.buffer "String"
  | Type.Prim Type.Unit ->
     Buffer.add_string pp.buffer "Unit"
  | Type.Var { contents = Solved ty } ->
     print_type pp parent_prec ty
  | Type.Var { contents = Wobbly wobbly } ->
     print_wobbly pp wobbly
  | Type.Var { contents = Rigid rigid } ->
     print_rigid pp rigid

let print_polytype pp (Type.Forall(tvars, ty)) =
  Buffer.add_string pp.buffer "forall ";
  List.iter tvars ~f:(fun tvar ->
      Buffer.add_char pp.buffer 't';
      Buffer.add_string pp.buffer (Int.to_string tvar.Type.rigid_id);
      begin match tvar.rigid_purity with
      | Type.Pure -> ()
      | Type.Impure ->
         Buffer.add_char pp.buffer '@';
         Buffer.add_string pp.buffer (Int.to_string tvar.rigid_lam_level)
      end;
      Buffer.add_char pp.buffer ' '
    );
  Buffer.add_char pp.buffer '.';
  print_type pp (-1) ty

let get_relevant_bindings env tctx _ty =
  let map = Env.to_map env in
  Map.fold map ~init:[] ~f:(fun ~key ~data acc ->
      match Hashtbl.find tctx data with
      | None -> acc
      | Some ty -> (key, ty)::acc
    )

let print_error pp = function
  | Message.Abstract_type id ->
     Buffer.add_string pp.buffer "Abstract type ";
     print_ident pp id
  | Message.Kind_unification_fail _ ->
     Buffer.add_string pp.buffer "Kind unification fail"
  | Message.Mismatched_arity ->
     Buffer.add_string pp.buffer "The pattern lists do not have the same arity."
  | Message.Missing_field name ->
     Buffer.add_string pp.buffer "Missing field ";
     Buffer.add_string pp.buffer name
  | Message.Not_enough_fields ->
     Buffer.add_string pp.buffer
       "This constructor pattern does not have enough fields."
  | Message.Occurs(wobbly, ty) ->
     Buffer.add_string pp.buffer "Occurs check: ";
     print_wobbly pp wobbly;
     Buffer.add_string pp.buffer " occurs in ";
     print_type pp (-1) ty
  | Message.Redefined_constr name ->
     Buffer.add_string pp.buffer "The constructor ";
     Buffer.add_string pp.buffer name;
     Buffer.add_string pp.buffer " has already been declared."
  | Message.Redefined_field_def name ->
     Buffer.add_string pp.buffer "Redefined field definition ";
     Buffer.add_string pp.buffer name
  | Message.Redefined_field name ->
     Buffer.add_string pp.buffer "The field ";
     Buffer.add_string pp.buffer name;
     Buffer.add_string pp.buffer " has already been defined."
  | Message.Redefined_name name ->
     Buffer.add_string pp.buffer "The name ";
     Buffer.add_string pp.buffer name;
     Buffer.add_string pp.buffer " has already been defined."
  | Message.Redefined_typevar name ->
     Buffer.add_string pp.buffer "The type variable ";
     Buffer.add_string pp.buffer name;
     Buffer.add_string pp.buffer " has already been declared."
  | Message.Syntax_error ->
     Buffer.add_string pp.buffer "Syntax error"
  | Message.Too_many_fields ->
     Buffer.add_string pp.buffer "This constructor pattern has too many fields."
  | Message.Type_unification_fail(t1, t2) ->
     Buffer.add_string pp.buffer "Type unification fail: ";
     print_type pp (-1) t1;
     Buffer.add_string pp.buffer " and ";
     print_type pp (-1) t2
  | Message.Typed_hole(env, tctx, ty) ->
     let rel_bindings = get_relevant_bindings env tctx ty in
     Buffer.add_string pp.buffer "Found typed hole:";
     newline pp;
     List.iter rel_bindings ~f:(fun (name, ty) ->
         Buffer.add_string pp.buffer name;
         Buffer.add_string pp.buffer " : ";
         print_polytype pp ty;
         newline pp
       );
     Buffer.add_string pp.buffer "_______________________";
     newline pp;
     print_type pp (-1) ty
  | Message.Unimplemented msg ->
     Buffer.add_string pp.buffer "Unimplemented ";
     Buffer.add_string pp.buffer msg
  | Message.Unknown_constr (_, name) ->
     Buffer.add_string pp.buffer "Unknown constructor ";
     Buffer.add_string pp.buffer name
  | Message.Unreachable_error str ->
     Buffer.add_string pp.buffer "Unreachable ";
     Buffer.add_string pp.buffer str
  | Message.Unresolved_field name ->
     Buffer.add_string pp.buffer "Unresolved_field ";
     Buffer.add_string pp.buffer name
  | Message.Unresolved_id id ->
     Buffer.add_string pp.buffer "Unresolved id ";
     print_ident pp id
  | Message.Unresolved_path path ->
     Buffer.add_string pp.buffer "Unresolved path ";
     print_path pp path
  | Message.Unresolved_type path ->
     Buffer.add_string pp.buffer "Unresolved type ";
     print_ident pp path
  | Message.Unresolved_typevar name ->
     Buffer.add_string pp.buffer "Unbound type variable ";
     Buffer.add_string pp.buffer name
  | Message.Unsafe_let_rec ->
     Buffer.add_string pp.buffer "This let rec definition is unsafe"
  | _ ->
     Buffer.add_string pp.buffer "other"

let print_pos pp pos =
  Buffer.add_string pp.buffer (Int.to_string pos.Lexing.pos_lnum);
  Buffer.add_string pp.buffer ":";
  Buffer.add_string pp.buffer
    (Int.to_string (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))

let print_span pp (start, fin) =
  print_pos pp start;
  Buffer.add_string pp.buffer " to ";
  print_pos pp fin;
  Buffer.add_string pp.buffer ": "

let rec print_message f pp e =
  begin match e with
  | Message.Diagnostic { Message.error; loc } ->
     f pp loc;
     print_error pp error
  | Message.And(l, r) ->
     print_message f pp l;
     print_message f pp r
  | Message.Lexer_error(pos, Message.Unclosed_comment) ->
     print_pos pp pos;
     Buffer.add_string pp.buffer ": Unclosed comment"
  | Message.Lexer_error(pos, Message.Unclosed_string) ->
     print_pos pp pos;
     Buffer.add_string pp.buffer ": Unclosed string literal";
  | Message.Unreachable str ->
     Buffer.add_string pp.buffer str
  end

let print_lit pp = function
  | Literal.Char ch ->
     Buffer.add_string pp.buffer (Char.escaped ch)
  | Literal.Float fl ->
     Buffer.add_string pp.buffer (Float.to_string fl)
  | Literal.Int i ->
     Buffer.add_string pp.buffer (Int.to_string i)
  | Literal.String str ->
     Buffer.add_char pp.buffer '\"';
     Buffer.add_string pp.buffer (String.escaped str);
     Buffer.add_char pp.buffer '\"'
  | Literal.Unit ->
     Buffer.add_string pp.buffer "()"

let rec print_comma_sep f pp = function
  | [] -> ()
  | [x] -> f pp x
  | x :: xs ->
     f pp x;
     Buffer.add_string pp.buffer ", ";
     print_comma_sep f pp xs

let print_label pp label =
  Buffer.add_string pp.buffer (Ir.Label.to_string label)

let print_procname pp name =
  Buffer.add_char pp.buffer 'F';
  Buffer.add_string pp.buffer (Int.to_string name)

module Ssa = struct
  let print_reg pp reg =
    Buffer.add_string pp.buffer (Ir.Register.to_string reg)

  let print_operand pp = function
    | Ir.Operand.Lit lit ->
       print_lit pp lit
    | Ir.Operand.Register id ->
       print_reg pp id

  let print_jump pp = function
    | Ssa.Break(label, args) ->
       print_label pp label;
       Buffer.add_char pp.buffer '(';
       print_comma_sep print_operand pp args;
       Buffer.add_char pp.buffer ')'
    | Ssa.Fail ->
       Buffer.add_string pp.buffer "panic"
    | Ssa.Return ->
       Buffer.add_string pp.buffer "return "
    | Ssa.Switch(scrut, cases, else_label) ->
       Buffer.add_string pp.buffer "switch ";
       print_operand pp scrut;
       Buffer.add_string pp.buffer " [";
       print_comma_sep (fun pp (case, conseq) ->
           Buffer.add_string pp.buffer (Int.to_string case);
           Buffer.add_string pp.buffer " -> ";
           print_label pp conseq
         ) pp cases;
       Buffer.add_string pp.buffer "] ";
       print_label pp else_label
    | Ssa.Tail_call(f, arg, args) ->
       Buffer.add_string pp.buffer "tail call ";
       print_operand pp f;
       Buffer.add_string pp.buffer " (";
       print_comma_sep print_operand pp (arg :: args);
       Buffer.add_string pp.buffer ")"

  let print_assn pp dest =
    print_reg pp dest;
    Buffer.add_string pp.buffer " = "

  let print_instr pp = function
    | Ssa.Assign(dest, lval, rval) ->
       print_assn pp dest;
       Buffer.add_string pp.buffer "assn ";
       print_operand pp lval;
       Buffer.add_char pp.buffer ' ';
       print_operand pp rval
    | Ssa.Box(dest, tag, items) ->
       print_assn pp dest;
       Buffer.add_string pp.buffer "box ";
       Buffer.add_string pp.buffer (Int.to_string tag);
       Buffer.add_string pp.buffer " [";
       print_comma_sep print_operand pp items;
       Buffer.add_char pp.buffer ']'
    | Ssa.Box_dummy(dest, size) ->
       print_assn pp dest;
       Buffer.add_string pp.buffer "dummy ";
       Buffer.add_string pp.buffer (Int.to_string size)
    | Ssa.Call(dest, f, arg, args) ->
       print_assn pp dest;
       Buffer.add_string pp.buffer "call ";
       print_operand pp f;
       Buffer.add_string pp.buffer " (";
       print_comma_sep print_operand pp (arg :: args);
       Buffer.add_string pp.buffer ")"
    | Ssa.Deref(dest, op) ->
       print_assn pp dest;
       Buffer.add_string pp.buffer "deref ";
       print_operand pp op
    | Ssa.Get(dest, op, idx) ->
       print_assn pp dest;
       Buffer.add_string pp.buffer "get ";
       print_operand pp op;
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer (Int.to_string idx)
    | Ssa.Load(dest, op) ->
       print_assn pp dest;
       Buffer.add_string pp.buffer "load ";
       print_operand pp op
    | Ssa.Package(dest, prefix) ->
       print_assn pp dest;
       Buffer.add_string pp.buffer "package ";
       print_prefix pp prefix
    | Ssa.Prim(dest, str) ->
       print_assn pp dest;
       Buffer.add_string pp.buffer "prim ";
       Buffer.add_string pp.buffer (String.escaped str)
    | Ssa.Ref(dest, op) ->
       print_assn pp dest;
       Buffer.add_string pp.buffer "ref ";
       print_operand pp op
    | Ssa.Set_field(dest, idx, op) ->
       Buffer.add_string pp.buffer "set_field ";
       print_operand pp dest;
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer (Int.to_string idx);
       Buffer.add_char pp.buffer ' ';
       print_operand pp op
    | Ssa.Set_tag(dest, tag) ->
       Buffer.add_string pp.buffer "set_tag ";
       print_operand pp dest;
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer (Int.to_string tag)
    | Ssa.Tag(dest, op) ->
       print_assn pp dest;
       Buffer.add_string pp.buffer "tag ";
       print_operand pp op

  let print_bb pp Ssa.{ params; preds; instrs; jump; _ } =
    Buffer.add_char pp.buffer '(';
    print_comma_sep print_reg pp params;
    Buffer.add_string pp.buffer "):";
    newline pp;
    Buffer.add_string pp.buffer "predecessors: ";
    print_comma_sep print_label pp (Set.to_list preds);
    newline pp;
    Queue.iter ~f:(fun instr ->
        print_instr pp instr;
        newline pp
      ) instrs;
    print_jump pp jump

  let print_proc pp
        Ssa.{ free_vars; params; blocks; before_returns; return; _ } =
    Buffer.add_char pp.buffer '[';
    print_comma_sep print_reg pp free_vars;
    Buffer.add_char pp.buffer ']';
    Buffer.add_char pp.buffer '(';
    print_comma_sep print_reg pp params;
    Buffer.add_char pp.buffer ')';
    indent pp (fun pp ->
        newline pp;
        Map.iteri ~f:(fun ~key ~data ->
            print_label pp key;
            indent pp (fun pp ->
                print_bb pp data
              );
            newline pp
          ) blocks;
        Buffer.add_string pp.buffer "return register: ";
        print_reg pp return;
        newline pp;
        Buffer.add_string pp.buffer "before returns: ";
        print_comma_sep print_label pp before_returns;
      );
    newline pp

  let print_module pp Ssa.{ procs; main } =
    Map.iteri ~f:(fun ~key ~data ->
        print_procname pp key;
        print_proc pp data;
        newline pp
      ) procs;
    Buffer.add_string pp.buffer "main";
    print_proc pp main
end

module Asm = struct
  let print_addr pp addr =
    Buffer.add_string pp.buffer @@ "%"^(Int.to_string addr)

  let print_operand pp = function
    | Asm.Lit lit -> print_lit pp lit
    | Asm.Stack addr -> print_addr pp addr

  let print_instr_args pp name dest args : unit =
    Buffer.add_string pp.buffer name;
    Buffer.add_char pp.buffer ' ';
    print_addr pp dest;
    List.iter args ~f:(fun arg ->
        Buffer.add_char pp.buffer ' ';
        print_operand pp arg;
      )

  let print_instr pp = function
    | Asm.Assign(dest, lval, rval) ->
       print_instr_args pp "assign" dest [lval; rval]
    | Asm.Box(dest, tag, operands) ->
       Buffer.add_string pp.buffer "box ";
       print_addr pp dest;
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer (Int.to_string tag);
       Buffer.add_string pp.buffer " [";
       print_comma_sep print_operand pp operands;
       Buffer.add_char pp.buffer ']'
    | Asm.Box_dummy(dest, size) ->
       Buffer.add_string pp.buffer "box_dummy ";
       print_addr pp dest;
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer (Int.to_string size)
    | Asm.Call(dest, f, arg, args) ->
       print_instr_args pp "call" dest [f];
       Buffer.add_char pp.buffer '(';
       print_comma_sep print_operand pp (arg::args);
       Buffer.add_char pp.buffer ')';
    | Asm.Deref(dest, ptr) ->
       print_instr_args pp "deref" dest [ptr]
    | Asm.Get(dest, value, idx) ->
       print_instr_args pp "get" dest [value];
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer @@ Int.to_string idx
    | Asm.Move(dest, src) ->
       print_instr_args pp "move" dest [src]
    | Asm.Package(dest, prefix) ->
       Buffer.add_string pp.buffer "package ";
       print_addr pp dest;
       Buffer.add_char pp.buffer ' ';
       print_prefix pp prefix
    | Asm.Prim(dest, primop) ->
       Buffer.add_string pp.buffer "prim ";
       print_addr pp dest;
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer @@ String.escaped primop
    | Asm.Ref(dest, value) ->
       print_instr_args pp "ref" dest [value]
    | Asm.Set_field(dest, idx, src) ->
       Buffer.add_string pp.buffer "set_field ";
       print_operand pp dest;
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer (Int.to_string idx);
       Buffer.add_char pp.buffer ' ';
       print_operand pp src
    | Asm.Set_tag(dest, tag) ->
       Buffer.add_string pp.buffer "set_tag ";
       print_operand pp dest;
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer (Int.to_string tag)
    | Asm.Tag(dest, op) ->
       print_instr_args pp "tag" dest [op]
    | Asm.Tail_call(f, arg, args) ->
       Buffer.add_string pp.buffer "tail_call";
       print_operand pp f;
       Buffer.add_char pp.buffer '(';
       print_comma_sep print_operand pp (arg::args);
       Buffer.add_char pp.buffer ')';
    | Asm.Break label ->
       Buffer.add_string pp.buffer "break ";
       print_label pp label
    | Asm.Fail ->
       Buffer.add_string pp.buffer "panic"
    | Asm.Return ->
       Buffer.add_string pp.buffer "return"
    | Asm.Switch(scrut, cases, else_case) ->
       Buffer.add_string pp.buffer "switch ";
       print_operand pp scrut;
       Buffer.add_string pp.buffer " [";
       print_comma_sep (fun pp (tag, label) ->
           Buffer.add_string pp.buffer (Int.to_string tag);
           Buffer.add_string pp.buffer " -> ";
           print_label pp label
         ) pp cases;
       Buffer.add_string pp.buffer "] ";
       print_label pp else_case

  let print_block pp block =
    Queue.iteri block.Asm.instrs ~f:(fun i instr ->
        print_instr pp instr;
        if i <> (Queue.length block.Asm.instrs - 1) then
          newline pp
      )

  let print_proc pp proc =
    Buffer.add_char pp.buffer '[';
    print_comma_sep print_addr pp proc.Asm.free_vars;
    Buffer.add_char pp.buffer ']';
    Buffer.add_char pp.buffer '(';
    print_comma_sep print_addr pp proc.Asm.params;
    Buffer.add_char pp.buffer ')';
    indent pp (fun pp ->
        newline pp;
        Buffer.add_string pp.buffer "max frame size: ";
        Buffer.add_string pp.buffer (Int.to_string proc.Asm.frame_size);
        newline pp;
        Map.iteri proc.Asm.blocks ~f:(fun ~key:label ~data:block ->
            print_label pp label;
            Buffer.add_char pp.buffer ':';
            indent pp (fun pp ->
                newline pp;
                print_block pp block
              );
            newline pp
          )
      );
    Buffer.add_string pp.buffer "return address: ";
    print_addr pp proc.Asm.return;
    newline pp

  let print_module pp package =
    Map.iteri package.Asm.procs ~f:(fun ~key:name ~data:proc ->
        print_procname pp name;
        print_proc pp proc;
        newline pp
      );
    Buffer.add_string pp.buffer "main";
    print_proc pp package.Asm.main
end
