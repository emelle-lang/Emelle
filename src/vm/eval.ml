(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
open Base

type box = {
    mutable tag : int;
    data : value array
  }

and partial_app = {
    proc : proc;
    frame_size : int;
    closure : value array;
    remaining_params : int list;
    param_arg_pairs : (int * value) list;
  }

and foreign = {
    arity : int;
    params : int list;
    f : value array -> value
  }

and value =
  | Box of box
  | Char of char
  | Float of float
  | Foreign of foreign
  | Partial_app of partial_app
  | Int of int
  | Ref of value ref
  | String of string
  | Uninitialized
  | Unit

and proc =
  | Emmeline_proc of Asm.proc
  | OCaml_proc of (value array -> value)

type frame = {
    data : value array;
    mutable block : Asm.block;
    mutable ip : int;
    proc : Asm.proc;
    mutable return : value;
  }

type t = {
    foreign_values : (string, value) Hashtbl.t;
    eval'd_packages : (Qual_id.Prefix.t, value) Hashtbl.t;
  }

let create io rt =
  { eval'd_packages = rt
  ; foreign_values =
      Hashtbl.of_alist_exn
        (module String)
        [ "putc"
        , Foreign
            { arity = 1
            ; params = [0]
            ; f = function
                  | [|Char c|] ->
                     io.Io.putc c;
                     Unit
                  | _ -> failwith "Type error" }
        ; "puts"
        , Foreign
            { arity = 1
            ; params = [0]
            ; f = function
                  | [|String s|] ->
                     io.Io.puts s;
                     Unit
                  | _ -> failwith "Type error" } ] }

let eval_operand frame = function
  | Asm.Lit (Literal.Char c) -> Char c
  | Asm.Lit (Literal.Float f) -> Float f
  | Asm.Lit (Literal.Int i) -> Int i
  | Asm.Lit (Literal.String s) -> String s
  | Asm.Lit Literal.Unit -> Unit
  | Asm.Stack addr -> frame.data.(addr)

let bump_ip frame =
  frame.ip <- frame.ip + 1

let break frame label =
  frame.block <- Map.find_exn frame.proc.Asm.blocks label;
  frame.ip <- 0

let rec eval_instr t file frame =
  match Queue.get frame.block.Asm.instrs frame.ip with
  | Asm.Assign(dest, lval, rval) ->
     begin match eval_operand frame lval with
     | Ref r -> r := eval_operand frame rval
     | _ -> failwith "Type error"
     end;
     frame.data.(dest) <- Unit;
     bump_ip frame
  | Asm.Box(dest, tag, ops) ->
     let data = Array.of_list (List.map ~f:(eval_operand frame) ops) in
     frame.data.(dest) <- Box { tag; data };
     bump_ip frame
  | Asm.Box_dummy(dest, size) ->
     let data = Array.create ~len:size Uninitialized in
     frame.data.(dest) <- Box { tag = 255; data };
     bump_ip frame
  | Asm.Call(dest, f, arg, args) ->
     let f = eval_operand frame f in
     let arg = eval_operand frame arg in
     let args = List.map ~f:(eval_operand frame) args in
     frame.data.(dest) <- apply_function t file f (arg::args);
     bump_ip frame
  | Asm.Deref(dest, r) ->
     begin match eval_operand frame r with
     | Ref r -> frame.data.(dest) <- !r
     | _ -> failwith "Type error"
     end;
     bump_ip frame
  | Asm.Get(dest, data, idx) ->
     begin match eval_operand frame data with
     | Box { data; _ } -> frame.data.(dest) <- data.(idx)
     | _ -> failwith "Type error"
     end;
     bump_ip frame
  | Asm.Move(dest, data) ->
     frame.data.(dest) <- eval_operand frame data;
     bump_ip frame
  | Asm.Package(dest, key) ->
     frame.data.(dest) <- Hashtbl.find_exn t.eval'd_packages key;
     bump_ip frame
  | Asm.Ref(dest, data) ->
     frame.data.(dest) <- Ref (ref (eval_operand frame data));
     bump_ip frame
  | Asm.Set_field(dest, idx, src) ->
     let src = eval_operand frame src in
     begin match eval_operand frame dest with
     | Box { data; _ } ->
        data.(idx) <- src;
     | _ -> failwith "Type error"
     end;
     bump_ip frame
  | Asm.Set_tag(dest, tag) ->
     begin match eval_operand frame dest with
     | Box box -> box.tag <- tag;
     | _ -> failwith "Type error"
     end;
     bump_ip frame
  | Asm.Tag(dest, addr) ->
     begin match eval_operand frame addr with
     | Box { tag; _ } -> frame.data.(dest) <- Int tag
     | _ -> failwith "Type error"
     end;
     bump_ip frame
  | Asm.Break label ->
     break frame label
  | Asm.Fail -> failwith "Pattern match failure"
  | Asm.Return op ->
     frame.return <- eval_operand frame op
  | Asm.Switch(scrut, cases, else_case) ->
     let scrut = eval_operand frame scrut in
     begin match scrut with
     | Int tag ->
        let rec check = function
          | [] -> break frame else_case
          | (i, conseq)::_ when i = tag -> break frame conseq
          | _::xs -> check xs in
        check cases
     | _ -> failwith "Unreachable"
     end
  | Asm.Prim(dest, key) ->
     frame.data.(dest) <- Hashtbl.find_exn t.foreign_values key;
     bump_ip frame

and apply_function t file value args =
  let proc, frame_size, proc_data, params, param_arg_pairs =
    match value with
    | Box { tag; data } when tag = Ir.function_tag ->
       begin match data.(0) with
       | Int idx ->
          let proc = Map.find_exn file.Asm.procs idx in
          Emmeline_proc proc, proc.Asm.frame_size, data, proc.Asm.params, []
       | _ -> failwith "Expected function pointer"
       end
    | Foreign { params; f; arity } -> OCaml_proc f, arity, [||], params, []
    | Partial_app { proc
                  ; frame_size
                  ; closure
                  ; remaining_params
                  ; param_arg_pairs } ->
       proc, frame_size, closure, remaining_params, param_arg_pairs
    | _ -> failwith "Type error: Expected function" in
  let execute param_arg_pairs =
    let data = Array.create ~len:frame_size Uninitialized in
    (* Load arguments into stack frame *)
    List.iter param_arg_pairs ~f:(fun (param, arg) ->
        data.(param) <- arg
      );
    match proc with
    | Emmeline_proc proc ->
       let frame =
         { data = data
         ; proc
         ; block = Map.find_exn proc.Asm.blocks proc.Asm.entry
         ; ip = 0
         ; return = Uninitialized } in
       (* Load environment into stack frame *)
       List.iteri proc.Asm.free_vars ~f:(fun i addr ->
           frame.data.(addr) <- proc_data.(i + 1)
         );
       let rec loop () =
         match frame.return with
         | Uninitialized ->
            eval_instr t file frame;
            loop ()
         | _ -> frame.return
       in loop ()
    | OCaml_proc proc -> proc data in
  let rec load_params acc params args =
    match params, args with
    | [], [] -> execute acc
    | param::params, arg::args ->
       load_params ((param, arg)::acc) params args
    | params, [] -> (* More parameters than arguments *)
       Partial_app
         { proc
         ; frame_size
         ; closure = proc_data
         ; remaining_params = params
         ; param_arg_pairs = acc }
    | [], args -> (* More arguments than parameters *)
       apply_function t file (execute acc) args
  in load_params param_arg_pairs params args

let eval t file =
  let proc = file.Asm.main in
  let frame =
    { data = Array.create ~len:proc.frame_size Uninitialized
    ; proc
    ; block = Map.find_exn proc.Asm.blocks proc.Asm.entry
    ; ip = 0
    ; return = Uninitialized } in
  let rec loop () =
    match frame.return with
    | Uninitialized ->
       eval_instr t file frame;
       loop ()
    | _ -> frame.return
  in loop ()
