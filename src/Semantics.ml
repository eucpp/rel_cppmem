open MiniKanren
open MiniKanrenStd

module type StepRelation =
  sig
    type tt
    type tl
    type ti = (tt, tl) MiniKanren.injected

    type st
    type sl
    type si = (st, sl) MiniKanren.injected

    type helper = ((tt * st) option, (tl * sl) MiniKanren.logic option MiniKanren.logic) MiniKanren.injected

    val (-->) : ti * si -> helper -> MiniKanren.goal
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

    type helper = ((tt * st) option, (tl * sl) MiniKanren.logic option MiniKanren.logic) MiniKanren.injected

    let (-->) t t' =
      fresh (t1 t2 a b)
        (S1.(-->) t t1)
        (S2.(-->) t t2)
        (conde [
          (t1 === Option.none ()) &&& (t' === t2);
          (t1 === Option.some a)  &&& (t2 === Option.none ()) &&& (t' === t1);
          (t1 === Option.some a)  &&& (t2 === Option.some b)  &&& ((t' === t1) ||| (t' === t2));
        ])

  end

module Make(S : StepRelation) =
  struct
    type tt = S.tt
    type tl = S.tl
    type ti = S.ti

    type st = S.st
    type sl = S.sl
    type si = S.si

    type helper = ((tt * st) option, (tl * sl) MiniKanren.logic option MiniKanren.logic) MiniKanren.injected

    let spaceo_norec spaceo t s t'' s'' = S.(
      fresh (res)
        ((t, s) --> res)
        (conde [
          (res === Option.none ()) &&& (t === t'') &&& (s === s'');

          fresh (t' s')
            (res === Option.some (Pair.pair t' s'))
            (delay @@ fun () -> spaceo t' s' t'' s'');
        ])
    )

    let (-->) = S.(-->)

    let (-->*) (t, s) (t', s') =
      let tbl  = make_table () in
      let relo = ref (fun t s t' s' -> assert false) in
      let spaceo_tabled = fun t s t' s' -> tabled4 tbl (spaceo_norec !relo) t s t' s' in
      relo := spaceo_tabled;
      spaceo_tabled t s t' s'
  end
