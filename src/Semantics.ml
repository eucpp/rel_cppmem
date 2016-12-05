open MiniKanren

module Make
  (T : Lang.Term) 
  (C : Lang.Context with type t = T.t with type lt = T.lt)
  (S : Lang.State) =
  struct
    type rule = (C.lc -> T.lt -> S.lt -> C.lc -> T.lt -> S.lt -> MiniKanren.goal)

    type t = (string * rule) list 

    let empty = []
    
    let make rules = rules

    let register rl rls = rl::rls
    let deregister = List.remove_assoc

    let stepo rls t s t' s' =
      fresh (c c' rdx rdx')
        (L.splito t  c  rdx )
        (L.splito t' c' rdx')
        (conde @@ List.map (fun (name, rl) -> rl c rdx s c' rdx' s') rls)
        
    let split t = run qr (fun q  r  -> splito (inj_term t) q r)
                         (fun qs rs -> Stream.zip (Stream.map prj_ctx qs) (Stream.map prj_term rs))

    let plug (c, t) = run q (fun q  -> splito q (inj_ctx c) (inj_term t))
                            (fun qs -> let 
                                         (hd, tl) = Stream.retrieve ~n:1 qs
                                       in
                                         (** Plugging should be deterministic *)
                                         assert (Stream.is_empty tl);
                                         prj_term @@ List.hd hd
                            )         
  end
