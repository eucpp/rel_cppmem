type loc = string
type tstmp = int

type mem_order = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

module Path : 
  sig
    type t = N | L of t | R of t
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
