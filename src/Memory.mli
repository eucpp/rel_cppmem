module ValueStorage :
  sig
    include Utils.Logic

    val allocate : Lang.Loc.ti list -> ti

    val from_assoc : (Lang.Loc.ti * Lang.Value.ti) list -> ti

    val shapeo : ti -> Lang.Loc.ti list -> MiniKanren.goal

    val reado  : ti ->       Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val writeo : ti -> ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
  end

module Timestamp :
  sig
    include Utils.Logic

    val ts : int -> ti

    val show : tl -> string

    val (<)  : ti -> ti -> MiniKanren.goal
    val (<=) : ti -> ti -> MiniKanren.goal
    val (>)  : ti -> ti -> MiniKanren.goal
    val (>=) : ti -> ti -> MiniKanren.goal
  end

module ViewFront :
  sig
    include Utils.Logic

    val bottom : unit -> ti

    val allocate : Lang.Loc.ti list -> ti

    val from_assoc : (Lang.Loc.ti * Timestamp.ti) list -> ti

    val shapeo : ti -> Lang.Loc.ti list -> MiniKanren.goal

    val tso     : ti ->       Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal
    val updateo : ti -> ti -> Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal

    val mergeo : ti -> ti -> ti -> MiniKanren.goal
  end

module ThreadFront :
  sig
    include Utils.Logic

    val allocate : Lang.Loc.ti list -> ti

    (** [tso thrd loc ts] obtains last timestamp [ts] at [loc] that was seen by thread [thrd] *)
    val tso : ti -> Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal

    (** [updateo thrd thrd' loc ts] updates thread's viewfronts at [loc] by new timestamp [ts] *)
    val updateo : ti -> ti -> Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal

    (** [front_relo thrd loc rel] obtains release front [rel] of thread [thrd] for location [loc] *)
    val front_relo : ti -> Lang.Loc.ti -> ViewFront.ti -> MiniKanren.goal

    (** [update_acqo thrd thrd' vf] joins acquire viewfront of thread [thrd] with [vf] and obtains [thrd']  *)
    val update_acqo : ti -> ti -> ViewFront.ti -> MiniKanren.goal

    (** [fence_acqo thrd thrd'] performs merge of thread's acquire front into its current front *)
    val fence_acqo : ti -> ti -> MiniKanren.goal

    (** [fence_relo thrd thrd'] performs merge of thread's current front into its release front for location [loc].
          If [loc] is absent then current viewfront is merged to all release fronts. *)
    val fence_relo : ?loc:Lang.Loc.ti -> ti -> ti -> MiniKanren.goal

    (** [promiseo thrd thrd' loc ts value vf ts_lb] makes new promise to write message {[loc]@[ts]=[value], [vf]} such that [ts_lb] < [ts] *)
    (* val promiseo : ti -> ti -> Lang.Loc.ti -> Timestamp.ti -> Lang.Value.ti -> ViewFront.ti -> MiniKanren.goal *)

    (** [fulfillo thrd thrd'] nondeterministically fulfills one of thread's promises *)
    (* val fulfillo : ti -> ti -> MiniKanren.goal

    val laggingo : ti -> MiniKanrenStd.Bool.groundi -> MiniKanren.goal

    val certifyo : ti -> MiniKanren.goal *)

    (** [spawno thrd thrd1 thrd2] spawns two new child threads with viewfronts equal to parent's viewfronts
          and local variables initialized to zeroes *)
    val spawno : ti -> ti -> ti -> MiniKanren.goal

    (** [joino thrd thrd' thrd1 thrd2] joins all viewfronts of [thrd1] and [thrd2]
          into corresponding viewfronts of parent [thrd]
          obtaining new parent thread [thrd'] *)
    val joino  : ti -> ti -> ti -> ti -> MiniKanren.goal
  end

module type ThreadLocalData =
  sig
    include Utils.Logic

    val spawno : ti -> ti -> ti -> MiniKanren.goal
    val joino  : ti -> ti -> ti -> ti -> MiniKanren.goal
  end

module ThreadLocalStorage(T : ThreadLocalData) :
  sig
    include Utils.Logic

    val nil   : unit -> ti
    val leaf  : T.ti -> ti
    val node  : ?left:ti -> ?right:ti -> T.ti -> ti

    val geto : ti       -> Lang.ThreadID.ti -> T.ti -> MiniKanren.goal
    val seto : ti -> ti -> Lang.ThreadID.ti -> T.ti -> MiniKanren.goal

    val spawno : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal
    val joino  : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal
  end

module LocStory :
  sig
    include Utils.Logic

    val allocate : unit -> ti

    val init : Lang.Value.ti -> ti

    (** [last_tso story ts] gets timestamp [ts] of last message written to [story] *)
    val last_tso : ti -> Timestamp.ti -> MiniKanren.goal

    (** [next_tso story ts] gets timestamp [ts] of next message to be written to [story] *)
    val next_tso : ti -> Timestamp.ti -> MiniKanren.goal

    (** [reado story ts_lb ts value vf ] obtains all messages [(ts, value, vf)] from [story]
          such that [ts_lb] <= [ts] *)
    val loado  : ti -> Timestamp.ti
                    -> Timestamp.ti -> Lang.Value.ti -> ViewFront.ti
                    -> MiniKanren.goal

    (** [writeo story story' value vf ] writes new message [(ts, value, vf)] to [story]
          such that for [ts] holds the relation [next_tso story ts] *)
    val storeo : ti -> ti -> Lang.Value.ti -> ViewFront.ti -> MiniKanren.goal

    val last_valueo : ti -> Lang.Value.ti -> MiniKanren.goal
  end

module MemStory :
  sig
    include Utils.Logic

    val allocate : Lang.Loc.ti list -> ti

    val init : (Lang.Loc.ti * Lang.Value.ti) list -> ti

    val shapeo : ti -> Lang.Loc.ti list -> MiniKanren.goal

    val snapshoto : ti -> (Lang.Loc.ti * Lang.Value.ti) list -> MiniKanren.goal

    val last_tso : ti -> Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal

    val next_tso : ti -> Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal

    val loado : ti -> Lang.Loc.ti -> Timestamp.ti
                   -> Timestamp.ti -> Lang.Value.ti -> ViewFront.ti
                   -> MiniKanren.goal

    val storeo : ti -> ti -> Lang.Loc.ti -> Lang.Value.ti -> ViewFront.ti -> MiniKanren.goal

    val last_valueo : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
  end
