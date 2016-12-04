open MiniKanren

module ET = Lang.ExprTerm
module EC = Lang.ExprContext 

module ST = Lang.StmtTerm
module SC = Lang.StmtContext

module Regs = Memory.Registers

module BasicExpr = 
  struct
    type t  = Lang.ExprLang.t
    type lt = Lang.ExprLang.lt

    type c  = Lang.ExprLang.c
    type lc = Lang.ExprLang.lc

    type s  = Lang.ExprLang.s
    type ls = Lang.ExprLang.ls

    type rule = (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)

    let (!) = MiniKanren.inj

    let varo' c t s c' t' s' = ET.(
      fresh (n x)
        (t  === !(Var x))
        (t' === !(Const n))
        (c  === c')
        (s  === s')
        (Regs.geto x s n)
    )

    let varo = ("var", varo') 

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
