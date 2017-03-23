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

    val inj : tt -> ti

    val from_list : (string * int) list -> tt
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

    val curro : ti -> ViewFront.ti -> MiniKanren.goal

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

    val create : (string * int) list -> (string * int) list -> tt

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

    val next_tso : ti -> MiniKanren.Nat.groundi -> MiniKanren.goal

    val read_acqo  : ti -> MiniKanren.Nat.groundi
                        -> MiniKanren.Nat.groundi -> MiniKanren.Nat.groundi -> ViewFront.ti
                        -> MiniKanren.goal

    val write_relo : ti -> ti -> MiniKanren.Nat.groundi -> ViewFront.ti -> MiniKanren.goal
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

    val read_acqo : ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi
                       -> MiniKanren.Nat.groundi -> MiniKanren.Nat.groundi -> ViewFront.ti -> MiniKanren.goal

    val write_relo : ti -> ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> ViewFront.ti -> MiniKanren.goal
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

    val get_localo : ti ->       Lang.Path.ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal
    val set_localo : ti -> ti -> Lang.Path.ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal

    val read_acqo  : ti -> ti -> Lang.Path.ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal
    val write_relo : ti -> ti -> Lang.Path.ti -> Lang.Loc.ti -> MiniKanren.Nat.groundi -> MiniKanren.goal

    (* val read_sco  : Path.lt -> loc logic -> Nat.logic -> lt -> lt -> goal
    val write_sco : Path.lt -> loc logic -> Nat.logic -> lt -> lt -> goal *)

    val spawno : ti -> ti -> Lang.Path.ti -> MiniKanren.goal
    val joino  : ti -> ti -> Lang.Path.ti -> MiniKanren.goal
  end
