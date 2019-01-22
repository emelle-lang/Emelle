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

let print_ident pp (package, name) =
  Buffer.add_string pp.buffer package;
  Buffer.add_string pp.buffer ".";
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
     Buffer.add_string pp.buffer "Ref";
  | Type.Prim Type.String ->
     Buffer.add_string pp.buffer "String"
  | Type.Var { ty = Some ty; _ } ->
     print_type pp parent_prec ty
  | Type.Var { id; quant; _ } ->
     begin match quant with
     | Type.Exists _ ->
        Buffer.add_char pp.buffer '_';
     | _ -> ()
     end;
     Buffer.add_char pp.buffer 't';
     Buffer.add_string pp.buffer (Int.to_string id)

let print_error pp e =
  begin match e with
  | Message.Abstract_type id ->
     Buffer.add_string pp.buffer "Abstract type ";
     print_ident pp id
  | Message.Kind_unification_fail _ ->
     Buffer.add_string pp.buffer "Kind unification fail"
  | Message.Type_unification_fail(t1, t2) ->
     Buffer.add_string pp.buffer "Type unification fail: ";
     print_type pp (-1) t1;
     Buffer.add_string pp.buffer " and ";
     print_type pp (-1) t2
  | Message.Unreachable str ->
     Buffer.add_string pp.buffer "Unreachable ";
     Buffer.add_string pp.buffer str
  | Message.Unresolved_id id ->
     Buffer.add_string pp.buffer "Unresolved id ";
     print_ident pp id
  | Message.Unresolved_path path ->
     Buffer.add_string pp.buffer "Unresolved path ";
     print_path pp path
  | Message.Unsafe_let_rec ->
     Buffer.add_string pp.buffer "Unsafe let rec"
  | _ ->
     Buffer.add_string pp.buffer "other"
  end;
  Buffer.add_char pp.buffer '\n'

let print_lit pp = function
  | Literal.Char ch ->
     Buffer.add_string pp.buffer (Char.escaped ch)
  | Literal.Float fl ->
     Buffer.add_string pp.buffer (Float.to_string fl)
  | Literal.Int i ->
     Buffer.add_string pp.buffer (Int.to_string i)
  | Literal.String str ->
     Buffer.add_string pp.buffer (String.escaped str)

let print_qual_id pp (package, name) =
  Buffer.add_string pp.buffer package;
  Buffer.add_char pp.buffer '.';
  Buffer.add_string pp.buffer name

let rec print_comma_sep f pp = function
  | [] -> ()
  | [x] -> f pp x
  | x::xs ->
     f pp x;
     Buffer.add_string pp.buffer ", ";
     print_comma_sep f pp xs

let print_label pp label =
  Buffer.add_char pp.buffer 'L';
  Buffer.add_string pp.buffer (Ir.Label.to_string label)

let print_procname pp name =
  Buffer.add_char pp.buffer 'F';
  Buffer.add_string pp.buffer (Int.to_string name)

module Ssa = struct
  let print_reg pp reg =
    Buffer.add_string pp.buffer "r%";
    Buffer.add_string pp.buffer (Ir.Register.to_string reg)

  let print_operand pp = function
    | Ir.Operand.Extern_var path ->
       print_qual_id pp path
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
    | Ssa.Return operand ->
       Buffer.add_string pp.buffer "return ";
       print_operand pp operand
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

  let print_opcode pp = function
    | Ssa.Assign(lval, rval) ->
       Buffer.add_string pp.buffer "assn ";
       print_operand pp lval;
       Buffer.add_char pp.buffer ' ';
       print_operand pp rval
    | Ssa.Box items ->
       Buffer.add_string pp.buffer "box [";
       print_comma_sep print_operand pp items;
       Buffer.add_char pp.buffer ']'
    | Ssa.Box_dummy size ->
       Buffer.add_string pp.buffer "dummy ";
       Buffer.add_string pp.buffer (Int.to_string size)
    | Ssa.Call(f, arg, args) ->
       Buffer.add_string pp.buffer "call ";
       print_operand pp f;
       Buffer.add_char pp.buffer ' ';
       print_operand pp arg;
       Buffer.add_string pp.buffer " [";
       List.iter ~f:(fun operand ->
           print_operand pp operand;
           Buffer.add_string pp.buffer "; "
         ) args;
       Buffer.add_string pp.buffer "]"
    | Ssa.Deref op ->
       Buffer.add_string pp.buffer "deref ";
       print_operand pp op
    | Ssa.Fun(f, captures) ->
       Buffer.add_string pp.buffer "closure ";
       print_procname pp f;
       Buffer.add_string pp.buffer " [";
       List.iter ~f:(fun capture ->
           print_operand pp capture;
           Buffer.add_string pp.buffer "; ";
         ) captures;
       Buffer.add_char pp.buffer ']'
    | Ssa.Get(op, idx) ->
       Buffer.add_string pp.buffer "get ";
       print_operand pp op;
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer (Int.to_string idx)
    | Ssa.Load op ->
       Buffer.add_string pp.buffer "load ";
       print_operand pp op
    | Ssa.Memcopy(dest, src) ->
       Buffer.add_string pp.buffer "memcopy ";
       print_operand pp dest;
       Buffer.add_char pp.buffer ' ';
       print_operand pp src
    | Ssa.Phi idx ->
       Buffer.add_string pp.buffer "phi ";
       Buffer.add_string pp.buffer (Int.to_string idx);
    | Ssa.Prim str ->
       Buffer.add_string pp.buffer "prim ";
       Buffer.add_string pp.buffer (String.escaped str)
    | Ssa.Ref op ->
       Buffer.add_string pp.buffer "ref ";
       print_operand pp op

  let print_instr pp Ssa.{ dest; opcode; _ } =
    print_reg pp dest;
    Buffer.add_string pp.buffer " = ";
    print_opcode pp opcode

  let print_bb pp Ssa.{ preds; instrs; jump; _ } =
    Buffer.add_string pp.buffer "predecessors: ";
    print_comma_sep print_label pp (Set.to_list preds);
    newline pp;
    Queue.iter ~f:(fun instr ->
        print_instr pp instr;
        newline pp
      ) instrs;
    print_jump pp jump

  let print_proc pp Ssa.{ free_vars; params; blocks; before_return; _ } =
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
            Buffer.add_char pp.buffer ':';
            indent pp (fun pp ->
                newline pp;
                print_bb pp data
              );
            newline pp
          ) blocks;
        Buffer.add_string pp.buffer "before return: ";
        print_label pp before_return;
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
    Buffer.add_string pp.buffer @@ Int.to_string addr

  let print_operand pp = function
    | Asm.Extern_var path -> print_qual_id pp path
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
    | Asm.Box(dest, operands) ->
       Buffer.add_string pp.buffer "box ";
       print_addr pp dest;
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
    | Asm.Fun(dest, procname, captures) ->
       print_instr_args pp "closure" dest [];
       Buffer.add_char pp.buffer ' ';
       print_procname pp procname;
       Buffer.add_char pp.buffer '(';
       print_comma_sep print_operand pp captures;
       Buffer.add_char pp.buffer ')'
    | Asm.Get(dest, value, idx) ->
       print_instr_args pp "get" dest [value];
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer @@ Int.to_string idx
    | Asm.Move(dest, src) ->
       print_instr_args pp "move" dest [src]
    | Asm.Memcopy(dest, mem_dest, src) ->
       print_instr_args pp "memcopy" dest [mem_dest; src]
    | Asm.Prim(dest, primop) ->
       Buffer.add_string pp.buffer "prim ";
       print_addr pp dest;
       Buffer.add_char pp.buffer ' ';
       Buffer.add_string pp.buffer @@ String.escaped primop
    | Asm.Ref(dest, value) ->
       print_instr_args pp "ref" dest [value]
    | Asm.Break label ->
       Buffer.add_string pp.buffer "break ";
       print_label pp label
    | Asm.Fail ->
       Buffer.add_string pp.buffer "panic"
    | Asm.Return retval ->
       Buffer.add_string pp.buffer "return ";
       print_operand pp retval
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
      )

  let print_module pp package =
    Map.iteri package.Asm.procs ~f:(fun ~key:name ~data:proc ->
        print_procname pp name;
        print_proc pp proc;
        newline pp
      );
    Buffer.add_string pp.buffer "main";
    print_proc pp package.Asm.main
end
