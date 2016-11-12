

module X = 
  struct
    type t = int
  end

module Y = 
  struct 
    type t = float
  end

module Z = 
  struct
    type t = X.t
    type u = Y.t
  end 

let f ((x, y) as z) s = z

type xx = (int -> string)

let _ = 
  f (1, 2) "pp"
