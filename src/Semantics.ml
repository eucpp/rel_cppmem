open MiniKanren

module type Step =
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
        ((t, s) ->? !!false) &&& (t === t'') &&& (s === s'');
        (fresh (t' s' rl)
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
