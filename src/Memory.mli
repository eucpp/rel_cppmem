module Registers :
  sig
    type tt = (string, MiniKanren.Nat.ground) VarList.tt
    type tl = (string MiniKanren.logic, MiniKanren.Nat.logic) VarList.tl
    type ti = (string, MiniKanren.Nat.ground, string MiniKanren.logic, MiniKanren.Nat.logic) VarList.ti
  end

module ViewFront :
  sig
    type tt = (string, MiniKanren.Nat.ground) VarList.tt
    type tl = (string MiniKanren.logic, MiniKanren.Nat.logic) VarList.tl
    type ti = (string, MiniKanren.Nat.ground, string MiniKanren.logic, MiniKanren.Nat.logic) VarList.ti
  end

module ThreadState :
  sig
    type tt

    type tl_inner

    type tl = tl_inner MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val inj : tt -> ti

    val create : (string * int) list -> (string * int) list -> tt

    val preallocate : string list -> string list -> tt

    val get_varo : ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal
    val set_varo : ti -> ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal

    val get_tso : ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal
    val set_tso : ti -> ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal

    (** [updateo thrd thrd' vf] joins viewfront of thread [thrd] with [vf] and obtains [thrd']  *)
    val updateo : ti -> ti -> ViewFront.ti -> MiniKanren.goal

    (** [joino thrd1 thrd2 vf] joins viewfronts of [thrd1] and [thrd2] into vf *)
    val joino  : ti -> ti -> ViewFront.ti -> MiniKanren.goal

    val spawno : ti -> ti -> ti -> MiniKanren.goal
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

    val geto : ti -> Lang.Path.ti -> ThreadState.ti -> MiniKanren.goal
    val seto : ti -> ti -> Lang.Path.ti -> ThreadState.ti -> MiniKanren.goal

    val spawno : ti -> ti -> Lang.Path.ti -> MiniKanren.goal
    val joino  : ti -> ti -> Lang.Path.ti -> MiniKanren.goal
  end

(*

module Cell :
  sig
    type t   = (tstmp * int * ViewFront.t)
    type lt' = (Nat.logic * Nat.logic * ViewFront.lt)

    type lt = lt' logic

    val inj : t -> lt
    val prj : lt -> t

    val show : t -> string
    val eq : t -> t -> bool
  end

module LocStory :
  sig
    type t
    type lt'
    type lt = lt' logic

    val empty : t

    val from_list : Cell.t list -> t

    val inj : t -> lt
    val prj : lt -> t

    val show : t -> string
    val eq : t -> t -> bool

    val next_tstmpo : lt -> Nat.logic -> goal

    val read_acqo  : lt -> Nat.logic -> Nat.logic -> Nat.logic -> ViewFront.lt -> goal
    val write_relo : Nat.logic -> ViewFront.lt -> lt -> lt -> goal

    val read_acq  : t -> tstmp -> Cell.t Stream.t
    val write_rel : int -> ViewFront.t -> t -> t
  end

module MemStory :
  sig
    type t
    type lt'
    type lt  = lt' logic

    val empty : t

    val from_assoc : (loc * LocStory.t) list -> t

    val inj : t -> lt

    val prj : lt -> t

    val show : t -> string

    val eq : t -> t -> bool

    val next_tstmpo : lt -> loc logic -> Nat.logic -> goal

    val read_acqo : lt -> loc logic -> Nat.logic -> Nat.logic -> Nat.logic -> ViewFront.lt -> goal

    val write_relo : loc logic -> Nat.logic -> ViewFront.lt -> lt -> lt -> goal

    val read_acq : t -> loc -> tstmp -> (tstmp * int * ViewFront.t) Stream.t

    val write_rel : loc -> int -> ViewFront.t -> t -> t

  end

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

module MemState :
  sig
    type t = {
      thrds : ThreadTree.t;
      story : MemStory.t;
      scmem : SCMemory.t
    }

    type lt' = {
      lthrds : ThreadTree.lt;
      lstory : MemStory.lt;
      lscmem : SCMemory.lt
    }

    type lt = lt' logic

    val empty : t

    val preallocate : string list -> string list -> t

    val inj : t -> lt
    val prj : lt -> t

    val show : t -> string
    val eq : t -> t -> bool

    val get_thrdo : Path.lt -> lt -> ThreadState.lt -> goal

    val assign_localo : Path.lt -> string logic -> Nat.logic -> lt -> lt -> goal

    val read_acqo  : Path.lt -> loc logic -> Nat.logic -> lt -> lt -> goal
    val write_relo : Path.lt -> loc logic -> Nat.logic -> lt -> lt -> goal

    val read_sco  : Path.lt -> loc logic -> Nat.logic -> lt -> lt -> goal
    val write_sco : Path.lt -> loc logic -> Nat.logic -> lt -> lt -> goal

    val spawn_thrdo : Path.lt -> lt -> lt -> goal
    val join_thrdo  : Path.lt -> lt -> lt -> goal

    val get_thrd : Path.t -> t -> ThreadState.t

    val assign_local : Path.t -> string -> int -> t -> t

    val read_acq  : Path.t -> string -> t -> (int * t) Stream.t
    val write_rel : Path.t -> string -> int -> t -> t
  end *)
