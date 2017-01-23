module BasicExpr :  
  sig
    type t  = Lang.ExprTerm.t
    type lt = Lang.ExprTerm.lt

    type c  = Lang.ExprContext.c
    type lc = Lang.ExprContext.lc

    type s  = Memory.ThreadState.t
    type ls = Memory.ThreadState.lt

    type rule = (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)

    val var   : string * rule
    val binop : string * rule

    val all   : (string * rule) list
  end

module BasicStmt :
  sig
    type t  = Lang.StmtTerm.t
    type lt = Lang.StmtTerm.lt

    type c  = Lang.StmtContext.c
    type lc = Lang.StmtContext.lc

    type s  = Memory.MemState.t
    type ls = Memory.MemState.lt

    type rule =  (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)
    
    val expr   : string * rule
    val pair   : string * rule
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
    type t  = Lang.StmtTerm.t
    type lt = Lang.StmtTerm.lt

    type c  = Lang.StmtContext.c
    type lc = Lang.StmtContext.lc

    type s  = Memory.MemState.t
    type ls = Memory.MemState.lt

    type rule =  (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)
    
    val read_acq  : string * rule
    val write_rel : string * rule
  end
