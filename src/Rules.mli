module Basic :
  sig
    type ti = Lang.Term.ti
    type ci = Lang.Context.ti
    type si = Memory.MemState.ti

    type rule =  (ci -> ti -> si -> ci -> ti -> si -> MiniKanren.goal)

    val var    : string * rule
    val binop  : string * rule
    val asgn   : string * rule
    val if'    : string * rule
    val repeat : string * rule
    val seq    : string * rule

    val all : (string * rule) list
  end

module ThreadSpawning :
  sig
    type ti = Lang.Term.ti
    type ci = Lang.Context.ti
    type si = Memory.MemState.ti

    type rule =  (ci -> ti -> si -> ci -> ti -> si -> MiniKanren.goal)

    val spawn  : string * rule
    val join   : string * rule

    val all : (string * rule) list
  end

module Rlx :
  sig
    type ti = Lang.Term.ti
    type ci = Lang.Context.ti
    type si = Memory.MemState.ti

    type rule =  (ci -> ti -> si -> ci -> ti -> si -> MiniKanren.goal)

    val read_rlx  : string * rule
    val write_rlx : string * rule

    val all : (string * rule) list
  end

module RelAcq :
  sig
    type ti = Lang.Term.ti
    type ci = Lang.Context.ti
    type si = Memory.MemState.ti

    type rule =  (ci -> ti -> si -> ci -> ti -> si -> MiniKanren.goal)

    val read_acq  : string * rule
    val write_rel : string * rule

    val all : (string * rule) list
  end

module Promise :
  sig
    type ti = Lang.Term.ti
    type ci = Lang.Context.ti
    type si = Memory.MemState.ti

    type rule =  (ci -> ti -> si -> ci -> ti -> si -> MiniKanren.goal)

    val promise : string * rule
    val fulfill : string * rule

    val all : (string * rule) list
  end
