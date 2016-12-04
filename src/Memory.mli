type loc   = string 
type tstmp = int 

type mem_order = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

val string_of_loc : loc -> string
val string_of_tstmp : tstmp -> string
val string_of_mo : mem_order -> string

module Path : 
  sig
    type t = N | L of t | R of t
  end 

module Registers : 
  sig
    type t  = (string * int) list 
    type lt = (string MiniKanren.logic * int MiniKanren.logic) MiniKanren.logic MiniKanren.List.logic
    
    val empty : t

    val inj : t -> lt
    val prj : lt -> t

    val show : t -> string
    val eq : t -> t -> bool

    val geto : string MiniKanren.logic -> lt -> int MiniKanren.logic -> MiniKanren.goal
    val seto : string MiniKanren.logic -> int MiniKanren.logic -> lt -> lt -> MiniKanren.goal

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
      curr : ViewFront.t;
    }

    val empty : t
  end

module ThreadTree : 
  sig
    type t

    val empty : t

    val get_thread : t -> Path.t -> ThreadState.t
    
    val update_thread : t -> Path.t -> ThreadState.t -> t
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
