open MiniKanren

module Make
  (T : Lang.Term) 
  (C : Lang.Context with type t = T.t with type lt' = T.lt')
  (S : Lang.State) =
  struct
    type rule = (C.lc -> T.lt -> S.lt -> C.lc -> T.lt -> S.lt -> MiniKanren.goal)

    type t = (string * rule) list 

    let empty = []
    
    let make rules = rules

    let register rl rls = rl::rls
    let deregister = List.remove_assoc

    let (!) = (!!)

    let stepo rls t s t' s' =
      fresh (c c' rdx rdx')
        (C.splito t  c  rdx )
        (C.reducibleo rdx !true)
        (conde @@ List.map (fun (name, rl) -> rl c rdx s c' rdx' s') rls)
        (rdx =/= rdx')
        (C.splito t' c' rdx')
        (* (conde [ *)
        (*   (b === !true) &&& *)
        (*   (C.reducibleo rdx !true) &&& *)
        (*   (conde @@ List.map (fun (name, rl) -> rl c rdx s c' rdx' s') rls) &&& *)
        (*   (rdx =/= rdx') &&& *)
        (*   (C.splito t' c' rdx'); *)
        
        (*   (b === !false) &&& *)
        (*   (C.reducibleo rdx !false); *)
        (* ]) *)
        

    let rec spaceo rls t s t'' s'' = 
      conde [
        (C.reducibleo t !false) &&& (t === t'') &&& (s === s'');
        (fresh (t' s')
          (C.reducibleo t !true)
          (stepo rls t s t' s') 
          (spaceo rls t' s' t'' s''));
      ]

        (* (stepo rls t s t' s' b) *)
        (* (conde [ *)
        (*   (b === !false) &&& (t === t'') &&& (s === s''); *)
        (*   (b === !true)  &&& () &&& (spaceo rls t' s' t'' s''); *)
        (* ]) *) 
        
    let split t = run qr (fun q  r  -> C.splito (T.inj t) q r)
                         (fun qs rs -> Stream.zip (Stream.map C.prj qs) (Stream.map T.prj rs))

    let plug (c, t) = run q (fun q  -> C.splito q (C.inj c) (T.inj t))
                            (fun qs -> T.prj @@ Utils.excl_answ qs)         

    let step rls t s = 
      run qr (fun q  r  -> stepo rls (T.inj t) (S.inj s) q r)
             (fun qs rs -> Stream.zip (Stream.map T.prj qs) (Stream.map S.prj rs))

    let space rls t s = 
      run qr (fun q  r  -> spaceo rls (T.inj t) (S.inj s) q r)
             (fun qs rs -> Stream.zip (Stream.map T.prj qs) (Stream.map S.prj rs))
  end
