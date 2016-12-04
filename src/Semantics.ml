open MiniKanren

module Make
  (L : Lang.Lang with type lt = Lang.ExprLang.lt with type lc = Lang.ExprLang.lc) =
  struct
    type rule = (L.lc -> L.lt -> L.ls -> L.lc -> L.lt -> L.ls -> MiniKanren.goal)

    type t = (string * rule) list 

    let empty = []
    
    let make rules = rules

    let register rl rls = rl::rls
    let deregister = List.remove_assoc

    let stepo rls t s t' s' =
      fresh (c c' rdx rdx')
        (L.splito t  c  rdx )
        (conde @@ List.map (fun (name, rl) -> rl c rdx s c' rdx' s') rls)
        (L.splito t' c' rdx')
            
  end
