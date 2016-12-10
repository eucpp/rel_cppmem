module BasicExpr :  
  sig
    type t  = Lang.ExprTerm.t
    type lt = Lang.ExprTerm.lt

    type c  = Lang.ExprContext.c
    type lc = Lang.ExprContext.lc

    type s  = Lang.ExprState.t
    type ls = Lang.ExprState.lt

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

    type s  = Lang.StmtState.t
    type ls = Lang.StmtState.lt

    type rule =  (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)
    
    val expr   : string * rule
    val asgn   : string * rule
    val if'    : string * rule
    val while' : string * rule
    val seq    : string * rule
  end
