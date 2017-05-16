open MiniKanren

module type Step =
  sig
    type tt
    type tl
    type ti = (tt, tl) MiniKanren.injected

    type st
    type sl
    type si = (st, sl) MiniKanren.injected

    val (->?) : ti -> MiniKanren.Bool.groundi -> MiniKanren.goal
    val (-->) : ti * si -> ti * si -> MiniKanren.goal
  end

module Make(S : Step) =
  struct
    type tt = S.tt
    type tl = S.tl
    type ti = S.ti

    type st = S.st
    type sl = S.sl
    type si = S.si

    let spaceo_norec spaceo t s t'' s'' = S.(
      conde [
        (t ->? !!false) &&& (t === t'') &&& (s === s'');
        (fresh (t' s' rl)
          (t ->? !!true)
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

(* let stepo reductiono (t, s) (t', s') =
  fresh (c c' rdx rdx')
    (splito t c rdx)
    (reducibleo rdx !!true)
    (rdx =/= rdx')
    (reductiono c rdx s c' rdx' s')
    (plugo t' c' rdx')

let reduction_relation rules c rdx s c' rdx' s' =
  let apply (name, rule) = rule c rdx s c' rdx' s' in
  conde @@ List.map apply rules

module BasicStep =
  struct
    type tt = Lang.Term.tt
    type tl = Lang.Term.tl
    type ti = (tt, tl) MiniKanren.injected

    type st = Memory.MemState.tt
    type sl = Memory.MemState.tl
    type si = (st, sl) MiniKanren.injected

    let (->?) = reducibleo
    let (-->) = stepo (reduction_relation Rules.Basic.all)
  end

module OperationalStep =
  struct
    type tt = Lang.Term.tt
    type tl = Lang.Term.tl
    type ti = (tt, tl) MiniKanren.injected

    type st = Memory.MemState.tt
    type sl = Memory.MemState.tl
    type si = (st, sl) MiniKanren.injected

    let (->?) = reducibleo
    let (-->) =
      let rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.Rlx.all @ Rules.RelAcq.all in
      stepo (reduction_relation rules)
  end *)
