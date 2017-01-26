module Basic :
  sig
    type t  = Lang.Term.t
    type lt = Lang.Term.lt

    type c  = Lang.Context.c
    type lc = Lang.Context.lc

    type s  = Memory.MemState.t
    type ls = Memory.MemState.lt

    type rule =  (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)

    val var    : string * rule
    val binop  : string * rule
    val asgn   : string * rule
    val if'    : string * rule
    val repeat : string * rule
    val seq    : string * rule
    val spawn  : string * rule
    val join   : string * rule

    val all : (string * rule) list 
  end

module RelAcq :
  sig
    type t  = Lang.Term.t
    type lt = Lang.Term.lt

    type c  = Lang.Context.c
    type lc = Lang.Context.lc

    type s  = Memory.MemState.t
    type ls = Memory.MemState.lt

    type rule =  (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)
    
    val read_acq  : string * rule
    val write_rel : string * rule
  end
