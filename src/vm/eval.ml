(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Base

type 'value box = {
    mutable tag : int;
    data : 'value array
  }

type 'value proc =
  | Emmeline_proc of Asm.proc
  | OCaml_proc of ('value array -> 'value)

type 'value partial_app = {
    proc : 'value proc;
    frame_size : int;
    closure : 'value array;
    remaining_params : int list;
    param_arg_pairs : (int * 'value) list;
  }

type 'value foreign = {
    arity : int;
    params : int list;
    f : 'value array -> 'value
  }

and 'a value = 'a constraint 'a = [>
  | `Box of 'a box
  | `Char of char
  | `Float of float
  | `Foreign of 'a foreign
  | `Partial_app of 'a partial_app
  | `Int of int
  | `Ref of 'a value ref
  | `String of string
  | `Uninitialized
  | `Unit
  ]

type 'a frame = {
    mutable data : 'a value array;
    mutable block : Asm.block;
    mutable ip : int;
    mutable proc : Asm.proc;
    mutable return_addr : int;
    mutable return : 'a value;
  }

type 'a t = {
    foreign_values : (string, 'a value) Hashtbl.t;
    eval'd_packages : (Qual_id.Prefix.t, 'a value) Hashtbl.t;
  }

type 'a fun_data = {
    fun_proc : 'a proc;
    fun_frame_size : int;
    fun_proc_data : 'a value array;
    fun_params : int list;
    fun_param_arg_pairs : (int * 'a value) list;
  }

type 'a arg_result =
  | Exact of (int * 'a) list
  | Too_few of 'a partial_app
  | Too_many of (int * 'a) list * 'a list

let create io rt =
  { eval'd_packages = rt
  ; foreign_values =
      Hashtbl.of_alist_exn (module String)
        [ "putc"
        , `Foreign
            { arity = 1
            ; params = [0]
            ; f = function
                  | [|`Char c|] ->
                     io.Io.putc c;
                     `Unit
                  | _ -> failwith "Type error" }
        ; "puts"
        , `Foreign
            { arity = 1
            ; params = [0]
            ; f = function
                  | [|`String s|] ->
                     io.Io.puts s;
                     `Unit
                  | _ -> failwith "Type error" } ] }

let add_foreign_fun t name data =
  Hashtbl.set t.foreign_values ~key:name ~data:(`Foreign data)

let foreign ~arity f =
  let rec helper acc = function
    | 0 -> []
    | n -> acc :: helper (acc + 1) (n - 1)
  in { arity; params = helper 0 arity; f }

let eval_operand frame = function
  | Asm.Lit (Literal.Char c) -> `Char c
  | Asm.Lit (Literal.Float f) -> `Float f
  | Asm.Lit (Literal.Int i) -> `Int i
  | Asm.Lit (Literal.String s) -> `String s
  | Asm.Lit Literal.Unit -> `Unit
  | Asm.Stack addr -> frame.data.(addr)

let bump_ip frame =
  frame.ip <- frame.ip + 1

let break frame label =
  frame.block <- Map.find_exn frame.proc.Asm.blocks label;
  frame.ip <- 0

(** Unpack a value into a function *)
let to_function file value =
  match value with
  | `Box { tag; data } when tag = Ir.function_tag ->
     begin match data.(0) with
     | `Int idx ->
        let proc = Map.find_exn file.Asm.procs idx in
        { fun_proc = Emmeline_proc proc
        ; fun_frame_size = proc.Asm.frame_size
        ; fun_proc_data = data
        ; fun_params = proc.Asm.params
        ; fun_param_arg_pairs = [] }
     | _ -> failwith "Expected function pointer"
     end
  | `Foreign { params; f; arity } ->
     { fun_proc = OCaml_proc f
     ; fun_frame_size = arity
     ; fun_proc_data = [||]
     ; fun_params = params
     ; fun_param_arg_pairs = [] }
  | `Partial_app { proc
                 ; frame_size
                 ; closure
                 ; remaining_params
                 ; param_arg_pairs } ->
     { fun_proc = proc
     ; fun_frame_size = frame_size
     ; fun_proc_data = closure
     ; fun_params = remaining_params
     ; fun_param_arg_pairs = param_arg_pairs }
  | _ -> failwith "Type error: Expected function"

let rec load_params fun_data param_arg_pairs params args =
  (* param_arg_pairs is the accumulated value *)
  match params, args with
  | [], [] -> Exact param_arg_pairs
  | param :: params, arg :: args ->
     load_params fun_data ((param, arg) :: param_arg_pairs) params args
  | params, [] ->
     (* More parameters than arguments *)
     Too_few
       { proc = fun_data.fun_proc
       ; frame_size = fun_data.fun_frame_size
       ; closure = fun_data.fun_proc_data
       ; remaining_params = params
       ; param_arg_pairs }
  | [], args ->
     (* More arguments than parameters *)
     Too_many(param_arg_pairs, args)

let rec tail_call t file frame fun_data args =
  let { fun_proc = proc
      ; fun_frame_size = frame_size
      ; fun_proc_data = proc_data
      ; fun_params = params
      ; fun_param_arg_pairs = param_arg_pairs } = fun_data in
  let tail_execute param_arg_pairs =
    match proc with
    | Emmeline_proc proc ->
       if Array.length frame.data < frame_size then (
         frame.data <- Array.create ~len:frame_size `Uninitialized
       ) else (
         for i = frame_size to Array.length frame.data - 1 do
           frame.data.(i) <- `Uninitialized
         done
       );
       frame.proc <- proc;
       frame.block <- Map.find_exn proc.Asm.blocks proc.Asm.entry;
       frame.ip <- 0;
       frame.return_addr <- proc.Asm.return;
       (* Load arguments into stack frame *)
       List.iter param_arg_pairs ~f:(fun (param, arg) ->
           frame.data.(param) <- arg
         );
       (* Load environment into stack frame *)
       List.iteri proc.Asm.free_vars ~f:(fun i addr ->
           frame.data.(addr) <- proc_data.(i + 1)
         )
    | OCaml_proc proc ->
       let data = Array.create ~len:frame_size `Uninitialized in
       (* Load arguments into stack frame *)
       List.iter param_arg_pairs ~f:(fun (param, arg) ->
           data.(param) <- arg
         );
       frame.return <- proc data
  in
  match load_params fun_data param_arg_pairs params args with
  | Exact param_arg_pairs ->
     tail_execute param_arg_pairs
  | Too_few partial_app ->
     frame.return <- `Partial_app partial_app
  | Too_many(param_arg_pairs, args) ->
     let f =
       execute_function t file proc proc_data frame_size param_arg_pairs
       |> to_function file
     in tail_call t file frame f args

and eval_instr t file frame =
  match Queue.get frame.block.Asm.instrs frame.ip with
  | Asm.Assign(dest, lval, rval) ->
     begin match eval_operand frame lval with
     | `Ref r -> r := eval_operand frame rval
     | _ -> failwith "Type error"
     end;
     frame.data.(dest) <- `Unit;
     bump_ip frame
  | Asm.Box(dest, tag, ops) ->
     let data = Array.of_list (List.map ~f:(eval_operand frame) ops) in
     frame.data.(dest) <- `Box { tag; data };
     bump_ip frame
  | Asm.Box_dummy(dest, size) ->
     let data = Array.create ~len:size `Uninitialized in
     frame.data.(dest) <- `Box { tag = 255; data };
     bump_ip frame
  | Asm.Call(dest, f, arg, args) ->
     let f = eval_operand frame f in
     let arg = eval_operand frame arg in
     let args = List.map ~f:(eval_operand frame) args in
     let f = to_function file f in
     frame.data.(dest) <- apply_function t file f (arg :: args);
     bump_ip frame
  | Asm.Deref(dest, r) ->
     begin match eval_operand frame r with
     | `Ref r -> frame.data.(dest) <- !r
     | _ -> failwith "Type error"
     end;
     bump_ip frame
  | Asm.Get(dest, data, idx) ->
     begin match eval_operand frame data with
     | `Box { data; _ } -> frame.data.(dest) <- data.(idx)
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
     frame.data.(dest) <- `Ref (ref (eval_operand frame data));
     bump_ip frame
  | Asm.Set_field(dest, idx, src) ->
     let src = eval_operand frame src in
     begin match eval_operand frame dest with
     | `Box { data; _ } ->
        data.(idx) <- src;
     | _ -> failwith "Type error"
     end;
     bump_ip frame
  | Asm.Set_tag(dest, tag) ->
     begin match eval_operand frame dest with
     | `Box box -> box.tag <- tag;
     | _ -> failwith "Type error"
     end;
     bump_ip frame
  | Asm.Tag(dest, addr) ->
     begin match eval_operand frame addr with
     | `Box { tag; _ } -> frame.data.(dest) <- `Int tag
     | _ -> failwith "Type error"
     end;
     bump_ip frame
  | Asm.Tail_call(f, arg, args) ->
     let f = eval_operand frame f in
     let arg = eval_operand frame arg in
     let args = List.map ~f:(eval_operand frame) args in
     let f = to_function file f in
     tail_call t file frame f (arg :: args)
  | Asm.Break label ->
     break frame label
  | Asm.Fail -> failwith "Pattern match failure"
  | Asm.Return ->
     frame.return <- frame.data.(frame.return_addr)
  | Asm.Switch(scrut, cases, else_case) ->
     let scrut = eval_operand frame scrut in
     begin match scrut with
     | `Int tag ->
        let rec check = function
          | [] -> break frame else_case
          | (i, conseq) :: _ when i = tag -> break frame conseq
          | _ :: xs -> check xs in
        check cases
     | _ -> failwith "Unreachable"
     end
  | Asm.Prim(dest, key) ->
     frame.data.(dest) <- Hashtbl.find_exn t.foreign_values key;
     bump_ip frame

(* Helper function used both by tail_call and apply_function *)
and execute_function t file proc proc_data frame_size param_arg_pairs =
  match proc with
  | Emmeline_proc proc ->
     let data = Array.create ~len:frame_size `Uninitialized in
     let frame =
       { data = data
       ; proc
       ; block = Map.find_exn proc.Asm.blocks proc.Asm.entry
       ; ip = 0
       ; return = `Uninitialized
       ; return_addr = proc.Asm.return } in
     (* Load arguments into stack frame *)
     List.iter param_arg_pairs ~f:(fun (param, arg) ->
         frame.data.(param) <- arg
       );
     (* Load environment into stack frame *)
     List.iteri proc.Asm.free_vars ~f:(fun i addr ->
         frame.data.(addr) <- proc_data.(i + 1)
       );
     let rec loop () =
       match frame.return with
       | `Uninitialized ->
          eval_instr t file frame;
          loop ()
       | _ -> frame.return
     in loop ()
  | OCaml_proc proc ->
     let data = Array.create ~len:frame_size `Uninitialized in
     (* Load arguments into stack frame *)
     List.iter param_arg_pairs ~f:(fun (param, arg) ->
         data.(param) <- arg
       );
     proc data

and apply_function t file fun_data args =
  let { fun_proc = proc
      ; fun_frame_size = frame_size
      ; fun_proc_data = proc_data
      ; fun_params = params
      ; fun_param_arg_pairs = param_arg_pairs } = fun_data in
  match load_params fun_data param_arg_pairs params args with
  | Exact param_arg_pairs ->
     execute_function t file proc proc_data frame_size param_arg_pairs
  | Too_few partial_app -> `Partial_app partial_app
  | Too_many(param_arg_pairs, args) ->
     let f =
       execute_function t file proc proc_data frame_size param_arg_pairs
       |> to_function file
     in apply_function t file f args

let eval t file =
  let proc = file.Asm.main in
  let frame =
    { data = Array.create ~len:proc.frame_size `Uninitialized
    ; proc
    ; block = Map.find_exn proc.Asm.blocks proc.Asm.entry
    ; ip = 0
    ; return = `Uninitialized
    ; return_addr = proc.Asm.return } in
  let rec loop () =
    match frame.return with
    | `Uninitialized ->
       eval_instr t file frame;
       loop ()
    | _ -> frame.return
  in loop ()
