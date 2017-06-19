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
    type sl = S1.sl)
  : StepRelation with
    type tt = S1.tt  and
    type tl = S1.tl  and
    type st = S1.st  and
    type sl = S1.sl

module Make(S : StepRelation) :
  sig
    type tt = S.tt
    type tl = S.tl
    type ti = S.ti

    type st = S.st
    type sl = S.sl
    type si = S.si

    val (->?)  : ti * si -> MiniKanren.Bool.groundi -> MiniKanren.goal
    val (-->)  : ti * si -> ti * si -> MiniKanren.goal
    val (-->*) : ti * si -> ti * si -> MiniKanren.goal
  end
