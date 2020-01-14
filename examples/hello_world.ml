let puts = foreign "puts" forall . String -> Unit

let () = puts "Hello, world!\n"
