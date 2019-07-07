export (puts)

let puts = foreign "puts" forall . String -> Unit
