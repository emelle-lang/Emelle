type coloring = {
    map : (Ir.Register.t, int) Base.Hashtbl.t;
    frame_size : int;
  }

type t = {
    colorings : (int, coloring, Base.Int.comparator_witness) Base.Map.t;
    main's_coloring : coloring;
  }

val handle_file : Ssa2.file -> (t, 'a Message.t) result
