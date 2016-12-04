module BasicExpr :  
  sig
    type t  = Lang.ExprLang.t
    type lt = Lang.ExprLang.lt

    type c  = Lang.ExprLang.c
    type lc = Lang.ExprLang.lc

    type s  = Lang.ExprLang.s
    type ls = Lang.ExprLang.ls

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
