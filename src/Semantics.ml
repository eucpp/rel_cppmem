open MiniKanren

module Semantics
  (T : Lang.Term)
  (C : Lang.Context with type t = T.t) :
  struct
    type rule = (C.lt -> C.lc -> C.ls -> C.lt -> C.lc -> C.ls -> MiniKanren.goal)

    type t = (string * rule) list 

    let empty = []
    
    let make rules = rules

    let register rl rls = rl::rls
    let deregister = List.remove_assoc

    (* let stepo rls t s t' s' = *)
  end
