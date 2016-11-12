module BasicExpr: 
  sig
    type t = Lang.ExprContext.t

    type c = Lang.ExprContext.c

    type s = Lang.ExprContext.s   

    type rresult = Lang.ExprContext.rresult

    val binop : c * t * s -> rresult
  end
