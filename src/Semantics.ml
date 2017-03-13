open MiniKanren

module Make
  (T : Lang.ATerm)
  (C : Lang.AContext with type t = T.t with type lt' = T.lt')
  (S : Lang.AState) =
  struct
    type rule = (C.lc -> T.lt -> S.lt -> C.lc -> T.lt -> S.lt -> MiniKanren.goal)

    type t = (string * rule) list

    let empty = []

    let make rules = rules

    let register rl rls = rl::rls
    let deregister = List.remove_assoc

    let (!) = (!!)

    let stepo rls t s t' s' rl =
      fresh (c c' rdx rdx')
        (C.splito t c rdx)
        (C.reducibleo rdx !true)
        (rdx =/= rdx')
        (conde @@ List.map (
            fun (name, rule) -> (rule c rdx s c' rdx' s') (*&&& (rl === inj name)*)
          ) rls)
        (C.splito t' c' rdx')


    let rec spaceo rls t s t'' s'' epath =
      conde [
        (C.reducibleo t !false) &&& (t === t'') &&& (s === s'');
        (fresh (t' s' rl)
          (C.reducibleo t !true)
          (stepo rls t s t' s' rl)
          (spaceo rls t' s' t'' s'' epath));
          (* (epath === rl % epath')); *)
      ]

    let reducible t = run q (fun q  -> C.reducibleo (T.inj t) q)
                            (fun qs -> !?(Utils.excl_answ qs))

    let split t = run qr (fun q  r  -> C.splito (T.inj t) q r)
                         (fun qs rs -> Stream.zip (Stream.map C.prj qs) (Stream.map T.prj rs))

    let plug (c, t) = run q (fun q  -> C.splito q (C.inj c) (T.inj t))
                            (fun qs -> T.prj @@ Utils.excl_answ qs)

    let step rls t s =
      run qr (fun q  r  -> fresh (rl) (stepo rls (T.inj t) (S.inj s) q r rl))
             (fun qs rs -> Stream.zip (Stream.map T.prj qs) (Stream.map S.prj rs))

    let space rls t s =
      run qr (fun q  r  -> fresh (epath) (spaceo rls (T.inj t) (S.inj s) q r epath))
             (fun qs rs -> Stream.zip (Stream.map T.prj qs) (Stream.map S.prj rs))
  end
