open MiniKanren

module type StepRelation =
  sig
    type tt
    type tl
    type ti = (tt, tl) MiniKanren.injected

    type st
    type sl
    type si = (st, sl) MiniKanren.injected

    val (->?) : ti * si -> MiniKanren.Bool.groundi -> MiniKanren.goal
    val (-->) : ti * si -> ti * si -> MiniKanren.goal
  end

  module UnionRelation
    (S1 : StepRelation)
    (S2 : StepRelation with
      type tt = S1.tt  and
      type tl = S1.tl  and
      type st = S1.st  and
      type sl = S1.sl) =
  struct
    type tt = S1.tt
    type tl = S1.tl
    type ti = (tt, tl) MiniKanren.injected

    type st = S1.st
    type sl = S1.sl
    type si = (st, sl) MiniKanren.injected

    let (->?) x b =
      fresh (b1 b2)
        (S1.(->?) x b1)
        (S2.(->?) x b2)
        (Bool.oro b1 b2 b)

    let (-->) a b = conde [
      S1.(-->) a b;
      S2.(-->) a b;
    ]

  end

module Make(S : StepRelation) =
  struct
    type tt = S.tt
    type tl = S.tl
    type ti = S.ti

    type st = S.st
    type sl = S.sl
    type si = S.si

    let spaceo_norec spaceo t s t'' s'' = S.(
      conde [
        ((t, s) ->? !!false) &&& (t === t'') &&& (s === s'');
        (* ((t, s) ->? !!true) &&& (t === t'') &&& (s === s''); *)
        (fresh (t' s')
          ((t, s) ->? !!true)
          ((t, s) --> (t', s'))
          (delay @@ fun () -> spaceo t' s' t'' s''));
      ]
    )

    let (->?) = S.(->?)
    let (-->) = S.(-->)

    let (-->*) (t, s) (t', s') =
      let tbl  = make_table () in
      let relo = ref (fun t s t' s' -> assert false) in
      let spaceo_tabled = fun t s t' s' -> tabled4 tbl (spaceo_norec !relo) t s t' s' in
      relo := spaceo_tabled;
      spaceo_tabled t s t' s'
  end
