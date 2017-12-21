module EventID :
  sig
    include Utils.Logic

    val eid : int -> ti
  end

module Event :
  sig
    include Utils.Logic

    val event : EventID.ti -> Lang.Label.ti -> ti
  end

module Order :
  sig
    include Utils.Logic

    val extendo : Event.ti * Event.ti -> ti -> ti -> MiniKanren.goal
  end

module SequentialConsistent :
  sig
    

  end
