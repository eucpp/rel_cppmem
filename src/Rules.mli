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
  end

(* module BasicStmt :  *)
(*   sig  *)
(*     type t  = Lang.StmtContext.t *)
(*     type lt = Lang.StmtContext.lt *)

(*     type c  = Lang.StmtContext.c *)
(*     type lc = Lang.StmtContext.lc *)

(*     type s *)
(*     type ls *)

(*     type rule =  (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal) *)
    
(*     val expro   : rule *)
(*     val asgno   : rule *)
(*     val ifo     : rule *)
(*     val repeato : rule *)
(*     val seqo    : rule *)
(*   end *)
