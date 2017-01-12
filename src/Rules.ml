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
      fresh (l r n path e)
        (c  === c')
        (t  === !(Asgn (l, r)))
        (t' === !Skip)
        (patho c path)
        (conde [
          fresh (x n)
            (l === !(AExpr !(ET.Var   x)))
            (r === !(AExpr !(ET.Const n)))
            (MemState.assign_localo path x n s s');
          fresh (x1 x2 n1 n2 s'')
            (l === !(Pair (!(ET.Var   x1), !(ET.Var   x2))))
            (r === !(Pair (!(ET.Const n1), !(ET.Const n2))))
            (MemState.assign_localo path x1 n1 s   s'')
            (MemState.assign_localo path x2 n2 s'' s' );
        ])
    ))

    let asgn = ("assign", asgno)
    
    let ifo c t s c' t' s' = ST.(SC.(
      fresh (e n btrue bfalse)
        (c === c')
        (s === s') 
        (t === !(If (!(AExpr e), btrue, bfalse)))
        (e === !(ET.Const n))
        (conde [
          (n =/= (inj_nat 0)) &&& (t' === btrue);
          (n === (inj_nat 0)) &&& (t' === bfalse);
        ])                                         
    ))

    let if' = ("if", ifo)

    let repeato c t s c' t' s' = ST.(SC.(
      fresh (body)
        (c  === c')
        (s  === s')
        (t  === !(Repeat body))
        (t' === !(If (body, t, !Skip)))
    ))

    let repeat = ("repeat", repeato)

    let seqo c t s c' t' s' = ST.(SC.(
      fresh (t1 t2)
        (s === s')
        (t === !(Seq (t1, t2)))
        (conde [
          (t1 === !Skip)  &&& (t' === t2)     &&& (c' === c);
          (t1 === !Stuck) &&& (t' === !Stuck) &&& (c' === !Hole);
        ])
    ))

   let seq = ("seq", seqo)

   let spawno c t s c' t' s' = ST.(SC.(
     fresh (l r path)
       (c  === c')
       (t  === !(Spw (l, r)))
       (patho c path)
       (t' === !(Par (l, r)))
       (MemState.spawn_thrdo path s s')
   ))

   let spawn = ("spawn", spawno)

   let joino c t s c' t' s' = ST.(SC.(
     fresh (t1 t2 n1 n2 path) 
       (c === c')
       (t1 === !(AExpr !(ET.Const n1)))
       (t2 === !(AExpr !(ET.Const n2)))
       (t  === !(Par  (t1, t2)))
       (t' === !(Pair (!(ET.Const n1), !(ET.Const n2))))
       (patho c path)
       (MemState.join_thrdo path s s')
   ))

   let join = ("join", joino)

   let all = [expr; asgn; if'; repeat; seq; spawn; join]

  end

module RelAcq =
  struct
    type t  = Lang.StmtTerm.t
    type lt = Lang.StmtTerm.lt

    type c  = Lang.StmtContext.c
    type lc = Lang.StmtContext.lc

    type s  = Lang.StmtState.t
    type ls = Lang.StmtState.lt

    type rule =  (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)

    module ExprSem = Semantics.Make(Lang.ExprTerm)(Lang.ExprContext)(Lang.ExprState)

    let expr_sem = ExprSem.make BasicExpr.all

    let (!) = (!!)

    let read_acqo c t s c' t' s' = ST.(SC.(
      fresh (l path n)
        (c  === c')
        (t  === !(Read (!ACQ, l)))
        (t' === !(AExpr !(ET.Const n)))
        (patho c path)
        (MemState.read_acqo path l n s s')
    )) 

    let read_acq = ("read_acq", read_acqo)

    let write_relo c t s c' t' s' = ST.(SC.(
      fresh (l n e es es' path)
        (c  === c')
        (t  === !(Write (!REL, l, e)))
        (t' === !Skip)
        (patho c path)
        (ExprSem.spaceo expr_sem e es !(ET.Const n) es')
        (MemState.write_relo path l n s s')
    ))

    let write_rel = ("write_rel", write_relo)
 
  end
