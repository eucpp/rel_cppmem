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

    val init : regs:string list -> mem:(string * int) list -> ti

    val regso : ti -> Lang.ThreadID.ti -> Memory.RegisterStorage.ti -> MiniKanren.goal

    val shapeo : ti -> Lang.Loc.ti list -> MiniKanren.goal

    val checko : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val transitiono : Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end

module State (M : Memory) :
  sig
    include Utils.Logic

    val mem   : M.ti -> ti
    val error : Error.ti -> M.ti -> ti

    val regso : ti -> Lang.ThreadID.ti -> Memory.RegisterStorage.ti -> MiniKanren.goal
    val transitiono : Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end

module type T =
  sig
    module Memory : Memory

    module State : module type of State(Memory)

    module Node : module type of Semantics.MakeConfig(Lang.Term)(State)

    val intrpo : (Lang.Term.tt, State.tt, State.tt, Lang.Term.tl, State.tl, State.tl) Semantics.interpreter

    (* val evalo : (Node.tt, Node.tt, Node.tl, Node.tl) Semantics.eval *)
  end

(* module SequentialConsistent :
  sig
    module State :
      sig
        include Semantics.State

        val init : regs:Lang.Register.ti list -> locs:Lang.Loc.ti list -> ti
      end

    module TLSNode : module type of Semantics.TLSNode(Lang.Term)(State)

    val evalo : (TLSNode.tt, TLSNode.tl) Semantics.eval
  end *)

module ReleaseAcquire : T
