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
    type lt = lt at MiniKanren.logic

    val inj : t -> lt
    val prj : lt -> t
  end 

module Registers : 
  sig
    type t   (* = (string * int) list  *)
    type lt'  
    type lt  = lt' MiniKanren.logic
    
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

    val empty : t

    val get : loc -> t -> int

    val update : loc -> tstmp -> t -> t
  end

module ThreadState :
  sig
    type t = {
      regs : Registers.t
      (* curr : ViewFront.t; *)
    }

    type lt' = {
      lregs : Registers.lt;
    }

    type lt = lt' MiniKanren.logic

    val empty : t

    val inj : t -> lt
    val prj : lt -> t

    val show : t -> string
    val eq : t -> t -> bool
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

    val get_thrd    : Path.t -> t -> ThreadState.t
    val update_thrd : Path.t -> ThreadState.t -> t -> t
  end

module History :
  sig
    type t

    val empty : t

    val last_tstmp : loc -> t -> tstmp
    
    val next_tstmp : loc -> t -> tstmp

    val get : loc -> tstmp -> t -> (loc * tstmp * int * ViewFront.t)

    val insert : loc -> tstmp -> int -> ViewFront.t -> t -> t 
  end

module StateST :
  sig
    type t = {
      history : History.t;
      thread  : ThreadState.t;
    }

    val empty : t
  end

module StateMT :
  sig
    type t = {
      history : History.t;
      tree    : ThreadTree.t;
    }

    val empty : t
  end
