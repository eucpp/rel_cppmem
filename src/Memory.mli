module Registers :
  sig
    type tt = (string, MiniKanren.Nat.ground) VarList.tt
    type tl = (string MiniKanren.logic, MiniKanren.Nat.logic) VarList.tl
    type ti = (string, MiniKanren.Nat.ground, string MiniKanren.logic, MiniKanren.Nat.logic) VarList.ti

    val to_logic   : tt -> tl

    val reseto : ti -> ti -> MiniKanren.goal

    val pprint : tl -> string
  end

module ViewFront :
  sig
    type tt = (string, MiniKanren.Nat.ground) VarList.tt
    type tl = (string MiniKanren.logic, MiniKanren.Nat.logic) VarList.tl
    type ti = (string, MiniKanren.Nat.ground, string MiniKanren.logic, MiniKanren.Nat.logic) VarList.ti

    val inj : tt -> ti

    val to_logic   : tt -> tl

    val from_list : (string * int) list -> tt

    val pprint : tl -> string
  end

module ThreadState :
  sig
    type tt

    type tl_inner

    type tl = tl_inner MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val inj : tt -> ti

    val to_logic   : tt -> tl

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
    val get_varo : ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal

    (** [set_varo thrd thrd' var value] performs write of thread-local variable *)
    val set_varo : ti -> ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal

    (** [last_tso thrd loc ts] obtains last timestamp [ts] at [loc] that was seen by thread [thrd] *)
    val last_tso : ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal

    (** [updateo thrd thrd' loc ts] updates thread's viewfronts at [loc] by new timestamp [ts] *)
    val updateo : ti -> ti -> Lang.Loc.ti -> Lang.Timestamp.ti -> MiniKanren.goal

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

    (** [spawno thrd thrd1 thrd2] spawns two new child threads with viewfronts equal to parent's viewfronts
          and local variables initialized to zeroes *)
    val spawno : ti -> ti -> ti -> MiniKanren.goal

    (** [joino thrd thrd' thrd1 thrd2] joins all viewfronts of [thrd1] and [thrd2]
          into corresponding viewfronts of parent [thrd]
          obtaining new parent thread [thrd'] *)
    val joino  : ti -> ti -> ti -> ti -> MiniKanren.goal
  end

module Threads :
  sig
    module Tree :
      sig
        type ('a, 't) t =
          | Nil
          | Node of 'a * 't * 't
      end

    type tt = (ThreadState.tt, tt) Tree.t
    type tl = (ThreadState.tl, tl) Tree.t MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    val inj : tt -> ti

    val create : ?rel: (string * int) list ->
                 ?acq: (string * int) list ->
                  (string * int) list ->
                  (string * int) list -> tt

    val pprint : tl -> string

    val geto : ti -> Lang.Path.ti -> ThreadState.ti -> MiniKanren.goal
    val seto : ti -> ti -> Lang.Path.ti -> ThreadState.ti -> MiniKanren.goal

    val spawno : ti -> ti -> Lang.Path.ti -> MiniKanren.goal
    val joino  : ti -> ti -> Lang.Path.ti -> MiniKanren.goal
  end

module LocStory :
  sig
    type tt

    type tl_inner

    type tl = tl_inner MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val inj : tt -> ti

    val create : int -> (int * int * ViewFront.tt) list -> tt

    val preallocate : string list -> tt

    val next_tso : ti -> Lang.Timestamp.ti -> MiniKanren.goal

    val reado  : ti -> Lang.Timestamp.ti
                    -> Lang.Timestamp.ti -> Lang.Value.ti -> ViewFront.ti
                    -> MiniKanren.goal

    val writeo : ti -> ti -> Lang.Value.ti -> ViewFront.ti -> MiniKanren.goal

    val last_valueo : ti -> Lang.Value.ti -> MiniKanren.goal
  end

module MemStory :
  sig
    type tt = (Lang.Loc.tt, LocStory.tt) VarList.tt
    type tl = (Lang.Loc.tl, LocStory.tl) VarList.tl
    type ti = (Lang.Loc.tt, LocStory.tt, Lang.Loc.tl, LocStory.tl) VarList.ti

    val inj : tt -> ti

    val create : (string * LocStory.tt) list -> tt

    val preallocate : string list -> tt

    val next_tso : ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal

    val reado : ti -> Lang.Loc.ti -> Lang.Timestamp.ti
                   -> Lang.Timestamp.ti -> Lang.Value.ti -> ViewFront.ti
                   -> MiniKanren.goal

    val writeo : ti -> ti -> Lang.Loc.ti -> Lang.Value.ti -> ViewFront.ti -> MiniKanren.goal

    val last_valueo : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
  end

(*
module SCMemory :
  sig
    type t
    type lt'
    type lt  = lt' logic

    val empty : t

    val preallocate : string list -> t

    val inj : t -> lt
    val prj : lt -> t

    val show : t -> string
    val eq : t -> t -> bool

    val geto : string MiniKanren.logic -> lt -> MiniKanren.Nat.logic -> MiniKanren.goal
    val seto : string MiniKanren.logic -> MiniKanren.Nat.logic -> lt -> lt -> MiniKanren.goal

    val get : string -> t -> int
    val set : string -> int -> t -> t
  end

*)

module MemState :
  sig
    type tt

    type tl_inner

    type tl = tl_inner MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val inj : tt -> ti

    val create : Threads.tt -> MemStory.tt -> tt

    val preallocate : string list -> string list -> tt

    val get_localo : ti ->       Lang.Path.ti -> Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal
    val set_localo : ti -> ti -> Lang.Path.ti -> Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal

    val read_rlxo  : ti -> ti -> Lang.Path.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val write_rlxo : ti -> ti -> Lang.Path.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val read_acqo  : ti -> ti -> Lang.Path.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val write_relo : ti -> ti -> Lang.Path.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val fence_acqo : ti -> ti -> Lang.Path.ti -> MiniKanren.goal
    val fence_relo : ti -> ti -> Lang.Path.ti -> MiniKanren.goal

    val last_valueo : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    (* val read_sco  : Path.lt -> loc logic -> Nat.logic -> lt -> lt -> goal
    val write_sco : Path.lt -> loc logic -> Nat.logic -> lt -> lt -> goal *)

    val spawno : ti -> ti -> Lang.Path.ti -> MiniKanren.goal
    val joino  : ti -> ti -> Lang.Path.ti -> MiniKanren.goal
  end
