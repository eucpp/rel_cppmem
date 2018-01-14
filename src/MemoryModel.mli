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

    val init : (string * int) list -> ti

    val shapeo : ti -> Lang.Loc.ti list -> MiniKanren.goal

    val checko : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val transitiono : Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end

module State (M : Memory) :
  sig
    include Utils.Logic

    val mem   : M.ti -> ti
    val error : Error.ti -> M.ti -> ti

    val transitiono : Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end

module type T =
  sig
    module Memory : Memory

    module State : module type of State(Memory)

    module Node : module type of Semantics.MakeConfig(Lang.Stmt)(State)

    val intrpo : (Lang.Stmt.tt, State.tt, State.tt, Lang.Stmt.tl, State.tl, State.tl) Semantics.interpreter

    (* val evalo : (Node.tt, Node.tt, Node.tl, Node.tl) Semantics.eval *)
  end

module SequentialConsistent : T

module ReleaseAcquire : T
