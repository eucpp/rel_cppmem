

(* module GlobalMemory :
  sig

  end *)

module State :
  sig
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.injected
  end

(** Sequential - interface of simpliest machine.
      The sequential machine should be capable to maintain a number of per-thread set of registers *)
module type Sequential =
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val reado  : ti ->       Lang.ThreadID.ti -> Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal
    val writeo : ti -> ti -> Lang.ThreadID.ti -> Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal
  end

(** Parallel - interface of machine that is able to spawn new threads or join existing threads  *)
module type Parallel =
  sig
    include Sequential

    val spawno : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal
    val joino  : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal
  end

(** NonAtomic - interface of machine that supports non-atomic access to shared memory.
      In addition to load/store operations it should be able to detect data-races between concurrent accesses. *)
module type NonAtomic =
  sig
    include Parallel

    val load_na  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val store_na : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val data_raceo : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
  end

(** SequentialConsistent - interface of machine that supports sequaltial-consistent accesses to shared memory.
      All sequaltial-consistent operations should be totally ordered. *)
module type SequentialConsistent =
  sig
    include Parallel

    val load_sc  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val store_sc : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
  end

(** ReleaseAcquire - interface of machine that supports acquire-stores and release-writes. *)
module type ReleaseAcquire =
  sig
    include Parallel

    val load_acq  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val store_rel : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
  end

(** Front machine - this machines is capable to simuate Release-Acquire (or stronger) memory model and also is able to detect data-races between non-atomic acesses.
      It supports sequaltial-consistent read/writes, release write and acquire read, non-atomic read/writes.
      Internally this machines maintains a history of all writes for each location.
      Every write is tagged with timestamp.
      Also for all thread it stores a number of `viewfronts` that restrict the set of writes that are visible to thread at given point in time.
  *)
module Front :
  sig
    include ReleaseAcquire

    val load_sc  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val store_sc : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val load_na  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val store_na : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val data_raceo : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
  end


(** [load s s' thrdId location value] - load command to machine that performs a read of [value] from [location] by thread with given [thrdId],
      [s] is an initial state of machine, [s'] possibly modified state (load instruction can modify internal representation of the state) *)
(* type ('tt, 'tl) load =
  ('tt, 'tl) State.ti -> ('tt, 'tl) State.ti -> Memory.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal *)

(** [store s s' thrdId location value] - store command to machine that performs a write of [value] to [location] by thread with given [thrdId],
      [s] is an initial state of machine, [s'] is modified state *)
(* type ('tt, 'tl) store =
  ('tt, 'tl) State.ti -> ('tt, 'tl) State.ti -> Memory.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal *)


(* module Promising :
  sig

  end *)
