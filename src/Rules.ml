open MiniKanren

module ET = Lang.ExprTerm
module EC = Lang.ExprContext 

module ST = Lang.StmtTerm
module SC = Lang.StmtContext

module Regs = Memory.Registers

module BasicExpr = 
  struct
    type t  = Lang.ExprTerm.t
    type lt = Lang.ExprTerm.lt

    type c  = Lang.ExprContext.c
    type lc = Lang.ExprContext.lc

    type s  = Lang.ExprState.t
    type ls = Lang.ExprState.lt

    type rule = (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)

    let (!) = MiniKanren.inj

    let varo c t s c' t' s' = ET.(
      fresh (n x)
        (c  === c')
        (s  === s')
        (t  === !(Var x))
        (t' === !(Const n))
        (Regs.geto x s n)
    )

    let var = ("var", varo)

    let binopo c t s c' t' s' = ET.(
      fresh (op x y z)
        (c  === c')
        (s  === s')
        (t  === !(Binop (op, !(Const x), !(Const y))))
        (t' === !(Const z))
        (conde [
          (op === !"+") &&& (Nat.addo x y z);
          (op === !"*") &&& (Nat.mulo x y z);
        ])       
    )

    let binop = ("binop", binopo)

    let all = [var; binop]
  end

(* module BasicStmt : *)
(*   struct *)
(*     type t  = Lang.StmtContext.t *)
(*     type lt = Lang.StmtContext.lt *)

(*     type c  = Lang.StmtContext.c *)
(*     type lc = Lang.StmtContext.lc *)

(*     type s *)
(*     type ls *)

(*     type rule = (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal) *)
    
(*     let expro c t s c' t' s' =  *)


(*     val asgno   : rule *)
(*     val ifo     : rule *)
(*     val repeato : rule *)
(*     val seqo    : rule *)
(*   end *)
