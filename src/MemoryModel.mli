module Error :
  sig
    type tt =
      | DataRace
      | AssertionFailed

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val reify : MiniKanren.helper -> ti -> tl

    val show : tl -> string
  end

module type Memory =
  sig
    include Utils.Logic

    val init : mem:(string * int) list -> ti

    val shapeo : ti -> Lang.Loc.ti list -> MiniKanren.goal

    val checko : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val stepo : Lang.ThreadID.ti -> Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end

module State (M : Memory) :
  sig
    include Utils.Logic

    val mem   : M.ti -> ti
    val error : Error.ti -> M.ti -> ti

    val stepo : Lang.ThreadID.ti -> Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end

module type T =
  sig
    module Memory : Memory

    module State : module type of State(Memory)

    module Node : module type of Semantics.MakeConfig(Lang.ThreadSubSys)(State)

    val patho : (Node.tt, Node.tl) Semantics.Reduction.path

    val intrpo : (Lang.ThreadSubSys.tt, State.tt, State.tt, Lang.ThreadSubSys.tl, State.tl, State.tl) Semantics.interpreter
  end

module SequentialConsistent : T

module ReleaseAcquire : T
