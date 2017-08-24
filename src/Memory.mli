module Storage :
  sig
    type ('at, 'bt) tt

    type ('al, 'bl) tl = ('al, 'bl) inner MiniKanren.logic
      and ('al, 'bl) inner

    type ('at, 'bt, 'al, 'bl) ti = (('at, 'bt) tt, ('al, 'bl) tl) MiniKanren.injected

    type ('at, 'al) key = ('at, 'al) MiniKanren.injected
    type ('bt, 'bl) value = ('bt, 'bl) MiniKanren.injected

    val empty : unit -> ('at, 'bt, 'al, 'bl) ti

    val allocate : ('bt, 'bl) value -> ('at, 'al) key list -> ('at, 'bt, 'al, 'bl) ti

    val from_assoc : (('at, 'al) key * ('bt, 'bl) value) list -> ('at, 'bt, 'al, 'bl) ti

    val reify :
      (MiniKanren.helper -> ('at, 'al) MiniKanren.injected -> 'al) ->
      (MiniKanren.helper -> ('bt, 'bl) MiniKanren.injected -> 'bl) ->
      MiniKanren.helper -> ('at, 'bt, 'al, 'bl) ti -> ('al, 'bl) tl

    val inj : ('at -> 'al) -> ('bt -> 'bl) -> ('at, 'bt) tt -> ('al, 'bl) tl

    val pprint : (Format.formatter -> 'al * 'bl -> unit) -> Format.formatter -> ('al, 'bl) tl -> unit

    val geto : ('at, 'bt, 'al, 'bl) ti ->                            ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal
    val seto : ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal

    val updateo :
      (('bt, 'bl) value -> ('bt, 'bl) value -> MiniKanren.goal) ->
      ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key -> MiniKanren.goal

    val mapo :
      (('at, 'al) key -> ('bt, 'bl) value -> ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal) ->
      ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> MiniKanren.goal

    val map2o :
      (('at, 'al) key -> ('bt, 'bl) value -> ('at, 'al) key -> ('bt, 'bl) value -> ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal) ->
      ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> MiniKanren.goal
  end

module RegisterStorage :
  sig
    type tt = (Lang.Register.tt, Lang.Value.tt) Storage.tt

    type tl = (Lang.Register.tl, Lang.Value.tl) Storage.tl

    type ti = (Lang.Register.tt, Lang.Value.tt, Lang.Register.tl, Lang.Value.tl) Storage.ti

    val allocate : Lang.Register.ti list -> ti

    val from_assoc : (Lang.Register.ti * Lang.Value.ti) list -> ti

    val reify : MiniKanren.helper -> ti -> tl

    val inj : tt -> tl

    val pprint : Format.formatter -> tl -> unit

    val reado  : ti ->       Lang.Register.ti -> Lang.Value.ti -> MiniKanren.goal
    val writeo : ti -> ti -> Lang.Register.ti -> Lang.Value.ti -> MiniKanren.goal

    val reseto : ti -> ti -> MiniKanren.goal
  end

module Timestamp :
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val ts : int -> ti

    val reify : MiniKanren.helper -> ti -> tl

    val inj : tt -> tl

    val show : tl -> string

    val (<)  : ti -> ti -> MiniKanren.goal
    val (<=) : ti -> ti -> MiniKanren.goal
    val (>)  : ti -> ti -> MiniKanren.goal
    val (>=) : ti -> ti -> MiniKanren.goal
  end

module ViewFront :
  sig
    type tt = (Lang.Loc.tt, Timestamp.tt) Storage.tt

    type tl = (Lang.Loc.tl, Timestamp.tl) Storage.tl

    type ti = (Lang.Loc.tt, Timestamp.tt, Lang.Loc.tl, Timestamp.tl) Storage.ti

    val bottom : unit -> ti

    val allocate : Lang.Loc.ti list -> ti

    val from_assoc : (Lang.Loc.ti * Timestamp.ti) list -> ti

    val reify : MiniKanren.helper -> ti -> tl

    val inj : tt -> tl

    val pprint : Format.formatter -> tl -> unit

    val tso     : ti ->       Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal
    val updateo : ti -> ti -> Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal

    val mergeo : ti -> ti -> ti -> MiniKanren.goal
  end

module ThreadFront :
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    (** [preallocate vars atomics] creates new thread front
          and allocates a storage for [registers] and viewfronts of [atomics] *)
    val preallocate : Lang.Register.ti list -> Lang.Loc.ti list -> ti

    val reify : MiniKanren.helper -> ti -> tl

    val inj : tt -> tl

    (* val create : ?rel: (string * int) list ->
                 ?acq: (string * int) list ->
                  (string * int) list ->
                  (string * int) list -> tt *)

    val pprint : Format.formatter -> tl -> unit

    (** [get_varo thrd var val] performs read of thread-local register *)
    val reado : ti -> Lang.Register.ti -> Lang.Value.ti -> MiniKanren.goal

    (** [set_varo thrd thrd' var value] performs write of thread-local register *)
    val writeo : ti -> ti -> Lang.Register.ti -> Lang.Value.ti -> MiniKanren.goal

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
    val leaf  : content -> ti
    val node  : ?left:ti -> ?right:ti -> content -> ti

    val geto : ti       -> Lang.ThreadID.ti -> T.ti -> MiniKanren.goal
    val seto : ti -> ti -> Lang.ThreadID.ti -> T.ti -> MiniKanren.goal

    val spawno : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal

    val joino  : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal
  end

module LocStory :
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val preallocate : Lang.Loc.ti list -> ti

    val reify : MiniKanren.helper -> ti -> tl

    val inj : tt -> tl

    (* val to_logic : tt -> tl *)

    (* val create : int -> (int * int * ViewFront.tt) list -> tt *)

    val pprint : Lang.Loc.tl -> Format.formatter -> tl -> unit

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
    type tt = (Lang.Loc.tt, LocStory.tt) Storage.tt

    type tl = (Lang.Loc.tl, LocStory.tl) Storage.tl

    type ti = (Lang.Loc.tt, LocStory.tt, Lang.Loc.tl, LocStory.tl) Storage.ti

    val preallocate : Lang.Loc.ti list -> ti

    val reify : MiniKanren.helper -> ti -> tl

    val inj : tt -> tl

    (* val to_logic : tt -> tl *)

    (* val create : (string * LocStory.tt) list -> tt *)

    val pprint : Format.formatter -> tl -> unit

    val last_tso : ti -> Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal

    val next_tso : ti -> Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal

    val loado : ti -> Lang.Loc.ti -> Timestamp.ti
                   -> Timestamp.ti -> Lang.Value.ti -> ViewFront.ti
                   -> MiniKanren.goal

    val storeo : ti -> ti -> Lang.Loc.ti -> Lang.Value.ti -> ViewFront.ti -> MiniKanren.goal

    val last_valueo : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
  end
