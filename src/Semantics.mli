module Term :
  sig
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.injected
  end

module Context :
  sig
    type ('ct, 'cl) ti = ('ct, 'cl) MiniKanren.injected
  end

module Split :
  sig
    module T :
      sig
        type ('t, 'c) t = {
          rdx : 't;
          ctx : 'c;
        }
      end

    type ('t, 'c) tt = ('t, 'c) T.t option
    type ('t, 'c) tl = ('t, 'c) T.t MiniKanren.logic option MiniKanren.logic

    type ('tt, 'ct, 'tl, 'cl) ti = (('tt, 'tl) tt, ('ct, 'cl) tl) MiniKanren.injected

    val none  : unit -> ('tt, 'ct, 'tl, 'cl) ti
    val split : ('tt, 'tl) MiniKanren.injected -> ('ct, 'cl) MiniKanren.injected -> ('tt, 'ct, 'tl, 'cl) ti

    val redexo   : ('tt, 'ct, 'tl, 'cl) ti -> ('tt, 'tl) MiniKanren.injected -> MiniKanren.goal
    val contexto : ('tt, 'ct, 'tl, 'cl) ti -> ('ct, 'cl) MiniKanren.injected -> MiniKanren.goal
  end

module Step :
  sig
    module T :
      sig
        type ('t, 'c) t = {
          term : 't;
           : 'c;
        }
      end


  end

type ('tt, 'ct, 'tl, 'cl) splitting = ('tt, 'tl) Term.ti -> ('tt, 'ct, 'tl, 'cl) Split.ti -> goal

type ('tt, 'ct, 'tl, 'cl) rule = ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> goal

type ('tt, 'tl, ) step =

val make_reduction_relation :
  ('tt, 'ct, 'tl, 'cl) splitting -> (string * ('tt, 'ct, 'tl, 'cl) rule) list ->

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

    type helper = ((tt * st) option, (tl * sl) MiniKanren.logic option MiniKanren.logic) MiniKanren.injected

    val (-->)  : ti * si -> helper -> MiniKanren.goal
    val (-->*) : ti * si -> ti * si -> MiniKanren.goal
  end
