
module EventID :
  sig
    include Utils.Logic
  end

module EventSet :
  sig
    include Utils.Logic
  end

module Ord :
  sig
    include Utils.Logic
  end

module PO : Ord

module PreExecution :
  sig
    module State :
      sig
        include Lang.MemoryModel

        val init : ThreadID.ti -> ti

        val eventso : ti -> EventSet.ti -> MiniKanren.goal
        val program_ordero : ti -> PO.ti -> MiniKanren.goal
      end

    val interpo : (Prog.tt, State.tt, State.tt, Prog.tl, State.tl, State.tl) Semantics.interpreter
  end

module SequentialConsistent :
  sig
    include Lang.MemoryModel
  end
