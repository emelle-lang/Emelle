type t

val compile_file : 'a Anf.file -> (Ssa.file, 'a Message.t) result
