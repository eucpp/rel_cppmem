open MiniKanren
open Memory
open Lang

open Lang.Term
open Lang.Context

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

    let varo c t s c' t' s' =
      fresh (n x path thrd)
        (c  === c')
        (s  === s')
        (t  === !(Var x))
        (t' === !(Const n))
        (patho c path)
        (MemState.get_thrdo path s thrd)
        (ThreadState.get_localo thrd x n)

    let var = ("var", varo)

    let binopo c t s c' t' s' =
      fresh (op x y z)
        (c  === c')
        (s  === s')
        (t  === !(Binop (op, !(Const x), !(Const y))))
        (t' === !(Const z))
        (conde [
          (op === !"+") &&& (Nat.addo x y z);
          (op === !"*") &&& (Nat.mulo x y z);
        ])

    let binop = ("binop", binopo)

    let asgno c t s c' t' s' =
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

    let asgn = ("assign", asgno)

    let ifo c t s c' t' s' =
      fresh (e n btrue bfalse)
        (c === c')
        (s === s')
        (t === !(If (!(Const n), btrue, bfalse)))
        (conde [
          (n =/= (inj_nat 0)) &&& (t' === btrue);
          (n === (inj_nat 0)) &&& (t' === bfalse);
        ])

    let if' = ("if", ifo)

    let repeato c t s c' t' s' =
      fresh (body)
        (c  === c')
        (s  === s')
        (t  === !(Repeat body))
        (t' === !(If (body, t, !Skip)))

    let repeat = ("repeat", repeato)

    let seqo c t s c' t' s' =
      fresh (t1 t2)
        (s === s')
        (t === !(Seq (t1, t2)))
        (conde [
          (t1 === !Skip)  &&& (t' === t2)     &&& (c' === c);
          (t1 === !Stuck) &&& (t' === !Stuck) &&& (c' === !Hole);
        ])

   let seq = ("seq", seqo)

   let spawno c t s c' t' s' =
     fresh (l r path)
       (c  === c')
       (t  === !(Spw (l, r)))
       (patho c path)
       (t' === !(Par (l, r)))
       (MemState.spawn_thrdo path s s')

   let spawn = ("spawn", spawno)

   let joino c t s c' t' s' =
     fresh (t1 t2 n1 n2 path)
       (c === c')
       (t1 === !(Const n1))
       (t2 === !(Const n2))
       (t  === !(Par  (t1, t2)))
       (t' === !(Pair (!(Const n1), !(Const n2))))
       (patho c path)
       (MemState.join_thrdo path s s')

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

    let read_acqo c t s c' t' s' =
      fresh (l path n)
        (c  === c')
        (t  === !(Read (!ACQ, l)))
        (t' === !(Const n))
        (patho c path)
        (MemState.read_acqo path l n s s')

    let read_acq = ("read_acq", read_acqo)

    let write_relo c t s c' t' s' =
      fresh (l n path)
        (c  === c')
        (t  === !(Write (!REL, l, !(Const n))))
        (t' === !Skip)
        (patho c path)
        (MemState.write_relo path l n s s')

    let write_rel = ("write_rel", write_relo)

    let all = [read_acq; write_rel; ]

  end

module SeqCons =
  struct
    type t  = Lang.Term.t
    type lt = Lang.Term.lt

    type c  = Lang.Context.c
    type lc = Lang.Context.lc

    type s  = Memory.MemState.t
    type ls = Memory.MemState.lt

    type rule =  (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)

    let (!) = (!!)

    let read_sco c t s c' t' s' =
      fresh (l path n)
        (c  === c')
        (t  === !(Read (!SC, l)))
        (t' === !(Const n))
        (patho c path)
        (MemState.read_sco path l n s s')

    let read_sc = ("read_sc", read_sco)

    let write_sco c t s c' t' s' =
      fresh (l n path)
        (c  === c')
        (t  === !(Write (!SC, l, !(Const n))))
        (t' === !Skip)
        (patho c path)
        (MemState.write_sco path l n s s')

    let write_sc = ("write_sc", write_sco)

    let all = [read_sc; write_sc;]
  end
