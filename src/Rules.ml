module ET = Lang.ExprTerm
module EC = Lang.ExprContext 

module ST = Lang.StmtTerm
module SC = Lang.StmtContext

module Regs = Memory.Registers

module BasicExpr: 
  sig
    type t  = Lang.StmtContext.t
    type lt = Lang.StmtContext.lt

    type c  = Lang.StmtContext.c
    type lc = Lang.StmtContext.lc

    type s  = Memory.Registers.t
    type ls = Memory.Registers.lt

    type rule = (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)

    let varo c t s c' t' s' = ET.(
      fresh (n x)
        (t  === !(Var x))
        (t' === !(Const n))
        (c  === c')
        (s  === s')
        (Regs.geto x s n)
    )

    (* val binopo : rule *)
  end

(* module BasicStmt :  *)
(*   sig  *)
(*     type t  = Lang.StmtContext.t *)
(*     type lt = Lang.StmtContext.lt *)

(*     type c  = Lang.StmtContext.c *)
(*     type lc = Lang.StmtContext.lc *)

(*     type s *)
(*     type ls *)

(*     type rule = (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal) *)
    
(*     val expro   : rule *)
(*     val asgno   : rule *)
(*     val ifo     : rule *)
(*     val repeato : rule *)
(*     val seqo    : rule *)
(*   end *)
