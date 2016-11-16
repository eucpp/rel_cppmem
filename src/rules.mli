module BasicExpr: 
  sig
    type t = Lang.ExprContext.t

    type c = Lang.ExprContext.c

    type s = Lang.ExprContext.s   

    type rresult = Lang.ExprContext.rresult

    val binop : c * t * s -> rresult list

    val read_na : c * t * s -> rresult list
  end

module BasicStmt :
  sig 
    type t = Lang.StmtContext.t

    type c = Lang.StmtContext.c

    type s = Lang.StmtContext.s   

    type rresult = Lang.StmtContext.rresult

    val write_na : c * t * s -> rresult list

    val assign : c * t * s -> rresult list
  end
