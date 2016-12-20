open MiniKanren

type loc   = string 
type tstmp = int 

type mem_order = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

val string_of_loc : loc -> string
val string_of_tstmp : tstmp -> string
val string_of_mo : mem_order -> string

module Path : 
  sig
    type 'a at = N | L of 'a | R of 'a

    type t  = t  at
    type lt = lt at logic

    val inj : t -> lt
    val prj : lt -> t
  end 

module Registers : 
  sig
    type t
    type lt'  
    type lt  = lt' logic
    
    val empty : t

    val inj : t -> lt
    val prj : lt -> t

    val show : t -> string
    val eq : t -> t -> bool

    val geto : string MiniKanren.logic -> lt -> MiniKanren.Nat.logic -> MiniKanren.goal
    val seto : string MiniKanren.logic -> MiniKanren.Nat.logic -> lt -> lt -> MiniKanren.goal

    val get : string -> t -> int
    val set : string -> int -> t -> t
  end

module ViewFront :
  sig
    type t
    type lt' 
    type lt = lt' MiniKanren.logic

    val empty : t

    val from_assoc : (loc * tstmp) list -> t

    val inj : t -> lt
    val prj : lt -> t

    val show : t -> string
    val eq : t -> t -> bool

    val geto    : loc MiniKanren.logic -> lt -> MiniKanren.Nat.logic -> MiniKanren.goal
    val updateo : loc MiniKanren.logic -> MiniKanren.Nat.logic -> lt -> lt -> MiniKanren.goal
    val joino   : lt -> lt -> lt -> MiniKanren.goal

    val get    : loc -> t -> tstmp
    val update : loc -> tstmp -> t -> t
    val join   : t -> t -> t
  end

module ThreadState :
  sig
    type t = {
      regs : Registers.t;
      curr : ViewFront.t;
    }

    type lt' = {
      lregs : Registers.lt;
      lcurr : ViewFront.lt;
    }

    type lt = lt' MiniKanren.logic

    val empty : t

    val inj : t -> lt
    val prj : lt -> t

    val show : t -> string
    val eq : t -> t -> bool

    val join_viewfronto : ViewFront.lt -> lt -> lt -> goal

    val get_localo    : lt -> string logic -> Nat.logic -> goal
    val assign_localo : string logic -> Nat.logic -> lt -> lt -> goal

    val get_tstmpo    : lt -> loc logic -> Nat.logic -> goal
    val update_tstmpo : loc logic -> Nat.logic -> lt -> lt -> ViewFront.lt -> goal 
    
    val spawno : lt -> lt -> lt -> goal
    val joino  : lt -> lt -> lt -> goal    
  end

module ThreadTree : 
  sig
    @type ('a, 't) at = Leaf of 'a | Node of 't * 't with gmap
 
    type t   = (ThreadState.t, t) at
    type lt' = (ThreadState.lt, lt' MiniKanren.logic) at
    type lt  = lt' MiniKanren.logic

    val empty : t

    val inj : t -> lt
    val prj : lt -> t

    val show : t -> string
    val eq : t -> t -> bool

    val get_thrdo    : Path.lt -> lt -> ThreadState.lt -> MiniKanren.goal
    val update_thrdo : Path.lt -> ThreadState.lt -> lt -> lt -> MiniKanren.goal      

    val spawn_thrdo : Path.lt -> lt -> lt -> MiniKanren.goal
    val join_thrdo  : Path.lt -> lt -> lt -> MiniKanren.goal

    val get_thrd    : Path.t -> t -> ThreadState.t
    val update_thrd : Path.t -> ThreadState.t -> t -> t

    val spawn_thrd : Path.t -> t -> t
    val join_thrd  : Path.t -> t -> t
  end

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

module MemState : 
  sig
    type t = {
      thrds : ThreadTree.t;
      story : MemStory.t;
    }

    type lt' = {
      lthrds : ThreadTree.lt;
      lstory : MemStory.lt;
    }

    type lt = lt' logic

    val empty : t

    val inj : t -> lt
    val prj : lt -> t

    val show : t -> string
    val eq : t -> t -> bool

    val get_thrdo : Path.lt -> lt -> ThreadState.lt -> goal 

    val assign_localo : Path.lt -> string logic -> Nat.logic -> lt -> lt -> goal

    val read_acqo  : Path.lt -> loc logic -> Nat.logic -> lt -> lt -> goal
    val write_relo : Path.lt -> loc logic -> Nat.logic -> lt -> lt -> goal

    val spawn_thrdo : Path.lt -> lt -> lt -> goal
    val join_thrdo  : Path.lt -> lt -> lt -> goal

    val get_thrd : Path.t -> t -> ThreadState.t

    val assign_local : Path.t -> string -> int -> t -> t

    val read_acq  : Path.t -> string -> t -> (int * t) Stream.t
    val write_rel : Path.t -> string -> int -> t -> t
  end
