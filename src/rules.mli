module BasicExpr: 
  sig
    type t = Lang.ExprContext.t

    type c = Lang.ExprContext.c

    type s = Lang.ExprContext.s   

    type rresult = Lang.ExprContext.rresult

    val const : t * s -> rresult
    val binop : t * s -> rresult
    (*val var : ExprContext.t * ExprContext.s -> ExprContext.rresult*)
  end
