module Graph =
  struct

  end

module SequentialConsistent =
  struct
    let lift_split splito term ctx rdx =
      fresh (term' rdx' graph)
        (term === Node.cfg term' graph)
        (rdx  === Node.cfg rdx'  graph)
        (splito term' ctx rdx')

    let lift_plug plugo ctx rdx term =
      fresh (term' rdx' graph)
        (term === Node.cfg term' graph)
        (rdx  === Node.cfg rdx'  graph)
        (plugo ctx rdx' term')

    let lift_rule rule ctx ctx' t t' =
      fresh (label prog prog' graph graph')
        (t  === Node.cfg prog  graph )
        (t' === Node.cfg prog' graph')
        (rule label ctx ctx' prog prog')
        (Graph.extendo label graph graph')

    let stepo = Semantics.Reduction.make_step
      (lift_split Lang.splito)
      (lift_plug Lang.plugo)
      (List.map lift_rule (Rules.Basic.all @ Rules.Atomic.all))

    let eval_thrdo =
      let irreducibleo t =
        fresh (prog graph)
          (t === Node.cfg prog graph)
          (Lang.Term.irreducibleo prog)
      in
      Semantics.Reduction.make_eval ~irreducibleo stepo

    let evalo t g =
      fresh (p1 p2 g1 g2)
        (t   === spw p1 p2)
        (t1  === Node.cfg p1 (Graph.empty ()))
        (t2  === Node.cfg p2 (Graph.empty ()))
        (t1' === Node.cfg (skip ()) g1)
        (t2' === Node.cfg (skip ()) g2)
        (eval_thrdo t1 t1')
        (eval_thrdo t2 t2')
        (Graph.merge g1 g2 g)

  end
