module Storage :
  sig
    type ('at, 'bt) tt

    type ('al, 'bl) tl = ('al, 'bl) inner MiniKanren.logic
      and ('al, 'bl) inner

    type ('at, 'bt, 'al, 'bl) ti = (('at, 'bt) tt, ('al, 'bl) tl) MiniKanren.injected

    type ('at, 'al) key = ('at, 'al) MiniKanren.injected
    type ('bt, 'bl) value = ('bt, 'bl) MiniKanren.injected

    val allocate : ('bt, 'bl) value -> ('at, 'al) key list -> ('at, 'bt, 'al, 'bl) ti

    val from_assoc : (('at, 'al) key * ('bt, 'bl) value) list -> ('at, 'bt, 'al, 'bl) ti

    val inj : ('at -> ('at, 'al) MiniKanren.injected) -> ('bt -> ('bt, 'bl) MiniKanren.injected) -> ('at, 'bt) tt -> ('at, 'bt, 'al, 'bl) ti

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

module ThreadLocalStorage :
  sig
    type 'at tt

    type 'al tl = 'al inner MiniKanren.logic
      and 'al inner

    type ('at, 'al) ti = ('at tt, 'al tl) MiniKanren.injected

    type ('at, 'al) content = ('at, 'al) MiniKanren.injected

    val nil   : unit -> ('at, 'al) ti
    val leaf  : ('at, 'al) content -> ('at, 'al) ti
    val node  : ?left:('at, 'al) ti -> ?right:('at, 'al) ti -> ('at, 'al) content -> ('at, 'al) ti

    val inj : ('at -> ('at, 'al) MiniKanren.injected) -> 'at tt -> ('at, 'al) ti

    val pprint : (Format.formatter -> 'al content -> unit) -> Format.formatter -> 'al tl -> unit

    val geto : ('at, 'al) ti                  -> Lang.ThreadID.ti -> ('at, 'al) content -> MiniKanren.goal
    val seto : ('at, 'al) ti -> ('at, 'al) ti -> Lang.ThreadID.ti -> ('at, 'al) content -> MiniKanren.goal

    val spawno :
      (('at, 'al) content -> ('at, 'al) content -> ('at, 'al) content -> MiniKanren.goal) ->
      ('at, 'al) ti -> ('at, 'al) ti -> Lang.ThreadID.ti -> MiniKanren.goal

    val joino  :
       (('at, 'al) content -> ('at, 'al) content -> ('at, 'al) content -> ('at, 'al) content -> MiniKanren.goal) ->
       ('at, 'al) ti -> ('at, 'al) ti -> Lang.ThreadID.ti -> MiniKanren.goal
  end

module Registers :
  sig
    type tt = (Lang.Var.tt, Lang.Value.tt) Storage.tt

    type tl = (Lang.Var.tl, Lang.Value.tl) Storage.tl

    type ti = (Lang.Var.tt, Lang.Value.tt, Lang.Var.tl, Lang.Value.tl) Storage.ti

    val allocate : Lang.Var.ti list -> ti

    val from_assoc : (Lang.Var.ti * Lang.Value.ti) list -> ti

    val inj : tt -> ti

    val pprint : Format.formatter -> tl -> unit

    val reado  : ti ->       Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal
    val writeo : ti -> ti -> Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal

    val reseto : ti -> ti -> MiniKanren.goal
  end

module Timestamp :
  sig
    type tt = MiniKanrenStd.Nat.ground
    type tl = MiniKanrenStd.Nat.logic
    type ti = MiniKanrenStd.Nat.groundi
  end

module ViewFront :
  sig
    type tt = (Lang.Loc.tt, Timestamp.tt) Storage.tt

    type tl = (Lang.Loc.tl, Timestamp.tl) Storage.tl

    type ti = (Lang.Loc.tt, Timestamp.tt, Lang.Loc.tl, Timestamp.tl) Storage.ti

    val bottom : unit -> ti

    val allocate : Lang.Loc.ti list -> ti

    val from_assoc : (Lang.Loc.ti * Lang.Value.ti) list -> ti

    val inj : tt -> ti

    val pprint : Format.formatter -> tl -> unit

    val tso     : ti ->       Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val updateo : ti -> ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val mergeo : ti -> ti -> ti -> MiniKanren.goal
  end

module ThreadState :
  sig
    type tt
    type tl
    type ti = (tt, tl) MiniKanren.injected

    val inj : tt -> ti

    val to_logic : tt -> tl

    val create : ?rel: (string * int) list ->
                 ?acq: (string * int) list ->
                  (string * int) list ->
                  (string * int) list -> tt

    (** [preallocate vars atomics] creates new thread state
          that has list of initialized local variables
          and initialized viewfronts for atomic variables  *)
    val preallocate : string list -> string list -> tt

    val pprint : tl -> string

    (** [get_varo thrd var val] performs read of thread-local variable *)
    val get_varo : ti -> Lang.Loc.ti -> MiniKanrenStd.Nat.groundi -> MiniKanren.goal

    (** [set_varo thrd thrd' var value] performs write of thread-local variable *)
    val set_varo : ti -> ti -> Lang.Loc.ti -> MiniKanrenStd.Nat.groundi -> MiniKanren.goal

    (** [tso thrd loc ts] obtains last timestamp [ts] at [loc] that was seen by thread [thrd] *)
    val tso : ti -> Lang.Loc.ti -> MiniKanrenStd.Nat.groundi -> MiniKanren.goal

    (** [updateo thrd thrd' loc ts] updates thread's viewfronts at [loc] by new timestamp [ts] *)
    val updateo : ti -> ti -> Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal

    (** [front_relo thrd loc rel] obtains release front [rel] of thread [thrd] for location [loc] *)
    val front_relo : ti -> Lang.Loc.ti -> ViewFront.ti -> MiniKanren.goal

    (** [update_acqo thrd thrd' vf] joins acquire viewfront of thread [thrd] with [vf] and obtains [thrd']  *)
    val update_acqo : ti -> ti -> ViewFront.ti -> MiniKanren.goal

    (** [fence_acqo thrd thrd'] performs merge of thread's acquire front into its current front *)
    val fence_acqo : ti -> ti -> MiniKanren.goal

    (** [fence_relo thrd thrd'] performs merge of thread's current front into all its release front *)
    val fence_relo : ti -> ti -> MiniKanren.goal

    (** [fence_loc_relo thrd thrd'] performs merge of thread's current front into its release front for location [loc] *)
    val fence_loc_relo : ti -> ti -> Lang.Loc.ti -> MiniKanren.goal

    (** [promiseo thrd thrd' loc ts value vf ts_lb] makes new promise to write message {[loc]@[ts]=[value], [vf]} such that [ts_lb] < [ts] *)
    val promiseo : ti -> ti -> Lang.Loc.ti -> Timestamp.ti -> Lang.Value.ti -> ViewFront.ti -> MiniKanren.goal

    (** [fulfillo thrd thrd'] nondeterministically fulfills one of thread's promises *)
    val fulfillo : ti -> ti -> MiniKanren.goal

    val laggingo : ti -> MiniKanrenStd.Bool.groundi -> MiniKanren.goal

    val certifyo : ti -> MiniKanren.goal

    (** [spawno thrd thrd1 thrd2] spawns two new child threads with viewfronts equal to parent's viewfronts
          and local variables initialized to zeroes *)
    val spawno : ti -> ti -> ti -> MiniKanren.goal

    (** [joino thrd thrd' thrd1 thrd2] joins all viewfronts of [thrd1] and [thrd2]
          into corresponding viewfronts of parent [thrd]
          obtaining new parent thread [thrd'] *)
    val joino  : ti -> ti -> ti -> ti -> MiniKanren.goal
  end

module LocStory :
  sig
    type tt
    type tl
    type ti = (tt, tl) MiniKanren.injected

    val inj : tt -> ti

    val to_logic : tt -> tl

    val create : int -> (int * int * ViewFront.tt) list -> tt

    val preallocate : string list -> tt

    val pprint : Lang.Loc.tl -> tl -> string

    (** [last_tso story ts] gets timestamp [ts] of last message written to [story] *)
    val last_tso : ti -> Timestamp.ti -> MiniKanren.goal

    (** [next_tso story ts] gets timestamp [ts] of next message to be written to [story] *)
    val next_tso : ti -> Timestamp.ti -> MiniKanren.goal

    (** [reado story ts_lb ts value vf ] obtains all messages [(ts, value, vf)] from [story]
          such that [ts_lb] <= [ts] *)
    val reado  : ti -> Timestamp.ti
                    -> Timestamp.ti -> Lang.Value.ti -> ViewFront.ti
                    -> MiniKanren.goal

    (** [writeo story story' value vf ] writes new message [(ts, value, vf)] to [story]
          such that for [ts] holds the relation [next_tso story ts] *)
    val writeo : ti -> ti -> Lang.Value.ti -> ViewFront.ti -> MiniKanren.goal

    val last_valueo : ti -> Lang.Value.ti -> MiniKanren.goal
  end

module MemStory :
  sig
    type tt = (Lang.Loc.tt, LocStory.tt) VarList.tt
    type tl = (Lang.Loc.tl, LocStory.tl) VarList.tl
    type ti = (Lang.Loc.tt, LocStory.tt, Lang.Loc.tl, LocStory.tl) VarList.ti

    val inj : tt -> ti

    val to_logic : tt -> tl

    val create : (string * LocStory.tt) list -> tt

    val preallocate : string list -> tt

    val pprint : tl -> string

    val last_tso : ti -> Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal

    val next_tso : ti -> Lang.Loc.ti -> Timestamp.ti -> MiniKanren.goal

    val reado : ti -> Lang.Loc.ti -> Timestamp.ti
                   -> Timestamp.ti -> Lang.Value.ti -> ViewFront.ti
                   -> MiniKanren.goal

    val writeo : ti -> ti -> Lang.Loc.ti -> Lang.Value.ti -> ViewFront.ti -> MiniKanren.goal

    val last_valueo : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
  end

module MemState :
  sig
    type tt

    type tl

    type ti = (tt, tl) MiniKanren.injected

    val inj : tt -> ti

    val to_logic : tt -> tl

    val refine : (tt, tl) MiniKanren.refined -> tl

    val create : ?na:ViewFront.tt -> ?sc:ViewFront.tt -> Threads.tt -> MemStory.tt -> tt

    val preallocate : string list -> string list -> tt

    val pprint : tl -> string

    val get_localo : ti ->       Lang.ThreadID.ti -> Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal
    val set_localo : ti -> ti -> Lang.ThreadID.ti -> Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal

    val read_nao  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> Timestamp.ti -> MiniKanren.goal
    val write_nao : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> Timestamp.ti -> MiniKanren.goal

    val read_na_dro  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> MiniKanren.goal
    val write_na_dro : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> MiniKanren.goal

    val read_dro  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> MiniKanren.goal
    val write_dro : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> MiniKanren.goal

    val read_rlxo  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> Timestamp.ti -> MiniKanren.goal
    val write_rlxo : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> Timestamp.ti -> MiniKanren.goal

    val read_acqo  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> Timestamp.ti -> MiniKanren.goal
    val write_relo : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> Timestamp.ti -> MiniKanren.goal

    val read_sco  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> Timestamp.ti -> MiniKanren.goal
    val write_sco : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> Timestamp.ti -> MiniKanren.goal

    val fence_acqo : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal
    val fence_relo : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal

    val promiseo : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val fulfillo : ti -> ti -> Lang.ThreadID.ti                       -> MiniKanren.goal

    val laggingo : ti -> MiniKanrenStd.Bool.groundi -> MiniKanren.goal

    val certifyo : ti -> Lang.ThreadID.ti -> MiniKanren.goal

    val last_valueo : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    (* val read_sco  : Lang.ThreadID.lt -> loc logic -> Nat.logic -> lt -> lt -> goal
    val write_sco : Lang.ThreadID.lt -> loc logic -> Nat.logic -> lt -> lt -> goal *)

    val spawno : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal
    val joino  : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal
  end
