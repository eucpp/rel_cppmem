module EventID :
  sig
    include Utils.Logic

    (* val eid : int -> ti *)
  end

module Event :
  sig
    include Utils.Logic

    val event : EventID.ti -> Lang.Label.ti -> ti
  end

module EventSet :
  sig
    include Utils.Logic
  end

module Order :
  sig
    include Utils.Logic
  end

module Graph :
  sig
    include Utils.Logic
  end

module SequentialConsistent :
  sig
    (* val evalo : Lang.Term.ti -> Graph.ti -> MiniKanren.goal *)

    (* val sc_execo : Lang.Term.ti -> EventSet.ti -> Order.ti -> MiniKanren.goal *)
    val sc_execo : Lang.Term.ti -> EventID.ti -> EventID.ti -> MiniKanren.goal
  end
