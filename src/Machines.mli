

(* module GlobalMemory :
  sig

  end *)

module State :
  sig
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.injected
  end

module type Sequential =
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val reado  : ti ->       Lang.ThreadID.ti -> Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal
    val writeo : ti -> ti -> Lang.ThreadID.ti -> Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal
  end

(** [load s s' thrdId location value] - load command to machine that performs a read of [value] from [location] by thread with given [thrdId],
      [s] is an initial state of machine, [s'] possibly modified state (load instruction can modify internal representation of the state) *)
(* type ('tt, 'tl) load =
  ('tt, 'tl) State.ti -> ('tt, 'tl) State.ti -> Memory.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal *)

(** [store s s' thrdId location value] - store command to machine that performs a write of [value] to [location] by thread with given [thrdId],
      [s] is an initial state of machine, [s'] is modified state *)
(* type ('tt, 'tl) store =
  ('tt, 'tl) State.ti -> ('tt, 'tl) State.ti -> Memory.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal *)

(* module type SC =
  sig
    type tt
    type tl

    val read_sco  : (tt, tl) load
    val write_sco : (tt, tl) store
  end *)

(** LocalCache machine - this machines is capable to simuate Release-Acquire (or stronger) memory model and also is able to detect data-races between non-atomic acesses.
      It supports sequaltial-consistent read/writes, release write and acquire read, non-atomic read/writes.
      Internally this machines maintains a history of all writes for each location.
      Every write is tagged with timestamp.
      Also for all thread it stores a number of `viewfronts` that restrict the set of writes that are visible to thread at given point in time.
  *)
(* module LocalCache :
  sig
    type tt
    type tl


  end *)

(* module Promising :
  sig

  end *)
