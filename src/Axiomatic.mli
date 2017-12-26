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

    type reified = (tt, tl) MiniKanren.reified
  end

module Order :
  sig
    include Utils.Logic

    type reified = (tt, tl) MiniKanren.reified
  end

module Graph :
  sig
    include Utils.Logic
  end

module SequentialConsistent :
  sig
    (* val evalo : Lang.Term.ti -> Graph.ti -> MiniKanren.goal *)

    (* val sc_execo : Lang.Term.ti -> EventSet.ti -> Order.ti -> MiniKanren.goal *)
    (* val sc_execo : Lang.Term.ti -> EventID.ti -> EventID.ti -> MiniKanren.goal *)
    val sc_exec : Lang.Term.ti -> (EventSet.reified * Order.reified) MiniKanren.Stream.t
  end
