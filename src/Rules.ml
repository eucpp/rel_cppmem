open MiniKanren
open Memory
open Lang
open MemOrder

open Term
open Context

module Basic =
  struct
    type ti = Term.ti
    type ci = Context.ti
    type si = MemState.ti

    type rule =  (ci -> ti -> si -> ci -> ti -> si -> MiniKanren.goal)

    let (!) = (!!)

    let asgno c t s c' t' s' =
      fresh (l r n path e)
        (c  === c')
        (t  === asgn l r)
        (t' === skip ())
        (patho c path)
        (conde [
          fresh (x n)
            (l === var x)
            (r === const n)
            (MemState.set_localo s s' path x n);
          fresh (x1 x2 n1 n2 s'')
            (l === pair (var   x1) (var   x2))
            (r === pair (const n1) (const n2))
            (MemState.set_localo s   s'' path x1 n1)
            (MemState.set_localo s'' s'  path x2 n2);
        ])

    let asgn = ("assign", asgno)

    let varo c t s c' t' s' =
      fresh (n x path thrd)
        (c  === c')
        (s  === s')
        (t  === var x)
        (t' === const n)
        (patho c path)
        (MemState.get_localo s path x n)

    let var = ("var", varo)

    let binopo c t s c' t' s' =
      fresh (op x y z)
        (c  === c')
        (s  === s')
        (t  === binop op (const x) (const y))
        (t' === const z)
        (conde [
          (op === !"+") &&& (Nat.addo x y z);
          (op === !"*") &&& (Nat.mulo x y z);
        ])

    let binop = ("binop", binopo)

    let repeato c t s c' t' s' =
      fresh (body)
        (c  === c')
        (s  === s')
        (t  === repeat body)
        (t' === if' body (skip ()) t)

    let repeat = ("repeat", repeato)

    let ifo c t s c' t' s' =
      fresh (e n btrue bfalse)
        (c === c')
        (s === s')
        (t === if' (const n) btrue bfalse)
        (conde [
          (n =/= (inj_nat 0)) &&& (t' === btrue);
          (n === (inj_nat 0)) &&& (t' === bfalse);
        ])

    let if' = ("if", ifo)

   let spawno c t s c' t' s' =
     fresh (l r path)
       (c  === c')
       (t  === spw l r)
       (t' === par l r)
       (patho c path)
       (MemState.spawno s s' path)

   let spawn = ("spawn", spawno)

   let joino c t s c' t' s' =
     fresh (t1 t2 n1 n2 path)
       (c === c')
       (t1 === const n1)
       (t2 === const n2)
       (t  === par t1 t2)
       (t' === pair (const n1) (const n2))
       (patho c path)
       (MemState.joino s s' path)

   let join = ("join", joino)

   let all = [var; binop; asgn; if'; repeat; spawn; join]

  end

module RelAcq =
  struct
    type ti = Lang.Term.ti
    type ci = Lang.Context.ti
    type si = Memory.MemState.ti

    type rule =  (ci -> ti -> si -> ci -> ti -> si -> MiniKanren.goal)

    let (!) = (!!)

    let read_acqo c t s c' t' s' =
      fresh (l path n)
        (c  === c')
        (t  === read !ACQ l)
        (t' === const n)
        (patho c path)
        (MemState.read_acqo s s' path l n)

    let read_acq = ("read_acq", read_acqo)

    let write_relo c t s c' t' s' =
      fresh (l n path)
        (c  === c')
        (t  === write !REL l (const n))
        (t' === skip ())
        (patho c path)
        (MemState.write_relo s s' path l n)

    let write_rel = ("write_rel", write_relo)

    let all = [read_acq; write_rel; ]

  end

(*
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
  end *)
