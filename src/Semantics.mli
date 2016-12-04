module Make
  (L : Lang.Lang with type lt = Lang.ExprLang.lt with type lc = Lang.ExprLang.lc) :
  sig
    type t

    type rule = (L.lc -> L.lt -> L.ls -> L.lc -> L.lt -> L.ls -> MiniKanren.goal)

    val empty : t
    
    val make : (string * rule) list -> t

    val register   : string * rule -> t -> t
    val deregister : string -> t -> t 

    val stepo : t -> L.lt -> L.ls -> L.lt -> L.ls -> MiniKanren.goal
  end
