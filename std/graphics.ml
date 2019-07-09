export (create)

type Canvas =

let create = foreign "create_canvas" forall . Unit -> Canvas
