open MiniKanren
open Memory

module ET = Lang.ExprTerm

module ST = Lang.StmtTerm
module SC = Lang.StmtContext

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
        (ThreadState.get_localo s x n)
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

module BasicStmt =
  struct
    type t  = Lang.StmtTerm.t
    type lt = Lang.StmtTerm.lt

    type c  = Lang.StmtContext.c
    type lc = Lang.StmtContext.lc

    type s  = Lang.StmtState.t
    type ls = Lang.StmtState.lt

    type rule = (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)
 
    let (!) = (!!)
      
    module ExprSem = Semantics.Make(Lang.ExprTerm)(Lang.ExprContext)(Lang.ExprState)

    let expr_sem = ExprSem.make BasicExpr.all

    let expro c t s c' t' s' = ST.(SC.(
      fresh (et es et' es' path)
        (c === c')
        (s === s')
        (t === !(AExpr et))
        (patho c path)
        (MemState.get_thrdo path s es)
        (ExprSem.spaceo expr_sem et es et' es')
        (t' === !(AExpr et'))
    ))

    let expr = ("expression", expro) 

    let asgno c t s c' t' s' = ST.(SC.(
      fresh (x n path e)
        (c  === c')
        (t  === !(Asgn (x, !(AExpr !(ET.Const n)))))
        (t' === !Skip)
        (patho c path)
        (MemState.assign_localo path x n s s')
    ))

    let asgn = ("assign", asgno)
    
    (* val ifo     : rule *)
    (* val repeato : rule *)
    (* val seqo    : rule *)
  end
