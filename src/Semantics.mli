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

module BasicStep :
  sig
    type tt = Lang.Term.tt
    type tl = Lang.Term.tl
    type ti = (tt, tl) MiniKanren.injected

    type st = Memory.MemState.tt
    type sl = Memory.MemState.tl
    type si = (st, sl) MiniKanren.injected

    val (->?) : ti -> MiniKanren.Bool.groundi -> MiniKanren.goal
    val (-->) : ti * si -> ti * si -> MiniKanren.goal
  end

module OperationalStep :
  sig
    type tt = Lang.Term.tt
    type tl = Lang.Term.tl
    type ti = (tt, tl) MiniKanren.injected

    type st = Memory.MemState.tt
    type sl = Memory.MemState.tl
    type si = (st, sl) MiniKanren.injected

    val (->?) : ti -> MiniKanren.Bool.groundi -> MiniKanren.goal
    val (-->) : ti * si -> ti * si -> MiniKanren.goal
  end

module Make(S : Step) :
  sig
    type tt = S.tt
    type tl = S.tl
    type ti = S.ti

    type st = S.st
    type sl = S.sl
    type si = S.si

    val (->?)  : ti -> MiniKanren.Bool.groundi -> MiniKanren.goal
    val (-->)  : ti * si -> ti * si -> MiniKanren.goal
    val (-->*) : ti * si -> ti * si -> MiniKanren.goal
  end
