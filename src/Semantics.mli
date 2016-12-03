module Semantics
  (T : Lang.Term)
  (C : Lang.Context with type t = T.t) :
  sig
    type t

    type rule = (C.lt -> C.lc -> C.ls -> C.lt -> C.lc -> C.ls -> MiniKanren.goal)

    val empty : t
    
    val make : (string * rule) list -> t

    val register   : string * rule -> t -> t
    val deregister : string -> t -> t 

    val stepo : t -> C.lt -> C.ls -> C.lt -> C.ls -> MiniKanren.goal
  end
