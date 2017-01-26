open MiniKanren
open Memory

module Basic =
  struct
    type t  = Lang.Term.t
    type lt = Lang.Term.lt

    type c  = Lang.Context.c
    type lc = Lang.Context.lc

    type s  = MemState.t
    type ls = MemState.lt

    type rule = (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)
 
    let (!) = (!!)

    let varo c t s c' t' s' = Lang.Term.(Lang.Context.(
      fresh (n x path thrd)
        (c  === c')
        (s  === s')
        (t  === !(Var x))
        (t' === !(Const n))
        (patho c path)
        (MemState.get_thrdo path s thrd)
        (ThreadState.get_localo thrd x n)
    ))

    let var = ("var", varo)

    let binopo c t s c' t' s' = Lang.Term.(Lang.Context.(
      fresh (op x y z)
        (c  === c')
        (s  === s')
        (t  === !(Binop (op, !(Const x), !(Const y))))
        (t' === !(Const z))
        (conde [
          (op === !"+") &&& (Nat.addo x y z);
          (op === !"*") &&& (Nat.mulo x y z);
        ])       
    ))

    let binop = ("binop", binopo)

    let asgno c t s c' t' s' = Lang.Term.(Lang.Context.(
      fresh (l r n path e)
        (c  === c')
        (t  === !(Asgn (l, r)))
        (t' === !Skip)
        (patho c path)
        (conde [
          fresh (x n)
            (l === !(Var   x))
            (r === !(Const n))
            (MemState.assign_localo path x n s s');
          fresh (x1 x2 n1 n2 s'')
            (l === !(Pair (!(Var   x1), !(Var   x2))))
            (r === !(Pair (!(Const n1), !(Const n2))))
            (MemState.assign_localo path x1 n1 s   s'')
            (MemState.assign_localo path x2 n2 s'' s' );
        ])
    ))

    let asgn = ("assign", asgno)
    
    let ifo c t s c' t' s' = Lang.Term.(Lang.Context.(
      fresh (e n btrue bfalse)
        (c === c')
        (s === s') 
        (t === !(If (!(Const n), btrue, bfalse)))
        (conde [
          (n =/= (inj_nat 0)) &&& (t' === btrue);
          (n === (inj_nat 0)) &&& (t' === bfalse);
        ])                                         
    ))

    let if' = ("if", ifo)

    let repeato c t s c' t' s' = Lang.Term.(Lang.Context.(
      fresh (body)
        (c  === c')
        (s  === s')
        (t  === !(Repeat body))
        (t' === !(If (body, t, !Skip)))
    ))

    let repeat = ("repeat", repeato)

    let seqo c t s c' t' s' = Lang.Term.(Lang.Context.(
      fresh (t1 t2)
        (s === s')
        (t === !(Seq (t1, t2)))
        (conde [
          (t1 === !Skip)  &&& (t' === t2)     &&& (c' === c);
          (t1 === !Stuck) &&& (t' === !Stuck) &&& (c' === !Hole);
        ])
    ))

   let seq = ("seq", seqo)

   let spawno c t s c' t' s' = Lang.Term.(Lang.Context.(
     fresh (l r path)
       (c  === c')
       (t  === !(Spw (l, r)))
       (patho c path)
       (t' === !(Par (l, r)))
       (MemState.spawn_thrdo path s s')
   ))

   let spawn = ("spawn", spawno)

   let joino c t s c' t' s' = Lang.Term.(Lang.Context.(
     fresh (t1 t2 n1 n2 path) 
       (c === c')
       (t1 === !(Const n1))
       (t2 === !(Const n2))
       (t  === !(Par  (t1, t2)))
       (t' === !(Pair (!(Const n1), !(Const n2))))
       (patho c path)
       (MemState.join_thrdo path s s')
   ))

   let join = ("join", joino)

   let all = [var; binop; asgn; if'; repeat; seq; spawn; join]

  end

module RelAcq =
  struct
    type t  = Lang.Term.t
    type lt = Lang.Term.lt

    type c  = Lang.Context.c
    type lc = Lang.Context.lc

    type s  = Memory.MemState.t
    type ls = Memory.MemState.lt

    type rule =  (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)

    let (!) = (!!)

    let read_acqo c t s c' t' s' = Lang.Term.(Lang.Context.(
      fresh (l path n)
        (c  === c')
        (t  === !(Read (!ACQ, l)))
        (t' === !(Const n))
        (patho c path)
        (MemState.read_acqo path l n s s')
    )) 

    let read_acq = ("read_acq", read_acqo)

    let write_relo c t s c' t' s' = Lang.Term.(Lang.Context.(
      fresh (l n e es es' path)
        (c  === c')
        (t  === !(Write (!REL, l, e)))
        (t' === !Skip)
        (patho c path)
        (* (ExprSem.spaceo expr_sem e es !(Const n) es') *)
        (MemState.write_relo path l n s s')
    ))

    let write_rel = ("write_rel", write_relo)

    let all = [read_acq; write_rel]
 
  end
