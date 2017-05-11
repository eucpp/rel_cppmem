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

    let rec asgno' l r s s' path = conde [
      fresh (x n)
        (l === var x)
        (r === const n)
        (MemState.set_localo s s' path x n);
      fresh (x1 t n1 e s'')
        (l === pair (var   x1) t)
        (r === pair (const n1) e)
        (MemState.set_localo s  s'' path x1 n1)
        (asgno' t e s'' s' path);
    ]

    let asgno c t s c' t' s' =
      fresh (l r n path e)
        (c  === c')
        (t  === asgn l r)
        (t' === skip ())
        (patho c path)
        (asgno' l r s s' path)

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
      fresh (op x y z )
        (c  === c')
        (s  === s')
        (t  === binop op (const x) (const y))
        (t' === const z)
        (conde [
          (op === !"+")  &&& (Nat.addo x y z);
          (op === !"*")  &&& (Nat.mulo x y z);
          (op === !"=")  &&& (conde [(x === y) &&& (z === (inj_nat 1)); (x =/= y) &&& (z === (inj_nat 0))]);
          (op === !"!=") &&& (conde [(x =/= y) &&& (z === (inj_nat 1)); (x === y) &&& (z === (inj_nat 0))]);
          (op === !"<")  &&& (conde [(Nat.lto x y !!true) &&& (z === (inj_nat 1)); (Nat.lto x y !!false) &&& (z === (inj_nat 0))]);
          (op === !"<=") &&& (conde [(Nat.leo x y !!true) &&& (z === (inj_nat 1)); (Nat.leo x y !!false) &&& (z === (inj_nat 0))]);
          (op === !">")  &&& (conde [(Nat.gto x y !!true) &&& (z === (inj_nat 1)); (Nat.gto x y !!false) &&& (z === (inj_nat 0))]);
          (op === !">=") &&& (conde [(Nat.geo x y !!true) &&& (z === (inj_nat 1)); (Nat.geo x y !!false) &&& (z === (inj_nat 0))]);
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

    let seqo c t s c' t' s' =
      (s === s') &&&
      (c === c') &&&
      (t === seq (skip ()) t')

    let seq = ("seq", seqo)

   let all = [var; binop; asgn; if'; repeat; seq;]

  end

module ThreadSpawning =
  struct
    type ti = Lang.Term.ti
    type ci = Lang.Context.ti
    type si = Memory.MemState.ti

    type rule =  (ci -> ti -> si -> ci -> ti -> si -> MiniKanren.goal)

    let spawno c t s c' t' s' =
      fresh (l r path)
       (c  === c')
       (t  === spw l r)
       (t' === par l r)
       (patho c path)
       (MemState.spawno s s' path)

    let spawn = ("spawn", spawno)

    let joino c t s c' t' s' =
      let rec expro t = conde [
        fresh (n)
          (t === const n);
        fresh (t1 t2)
          (t === pair t1 t2)
          (expro t1)
          (expro t2);
      ] in
      fresh (t1 t2 path)
        (t === par t1 t2)
        (conde [
          (t1 === stuck ())                       &&& (t' === stuck ()) &&& (c' === hole ());
          (t1 =/= stuck ()) &&& (t2 === stuck ()) &&& (t' === stuck ()) &&& (c' === hole ());
          (t1 =/= stuck ()) &&& (t2 =/= stuck ()) &&& (c === c') &&& (conde [
            (t1 === skip ()) &&& (expro t2) &&& (t' === t2);
            (expro t1) &&& (t2 === skip ()) &&& (t' === t1);
            (expro t1) &&& (expro t2)       &&& (t' === pair t1 t2);
          ])
        ])
        (patho c path)
        (MemState.joino s s' path)

   let join = ("join", joino)

   let all = [spawn; join]
  end

module Rlx =
  struct
    type ti = Lang.Term.ti
    type ci = Lang.Context.ti
    type si = Memory.MemState.ti

    type rule =  (ci -> ti -> si -> ci -> ti -> si -> MiniKanren.goal)

    let read_rlxo c t s c' t' s' =
      fresh (l path n)
        (c  === c')
        (t  === read !!RLX l)
        (t' === const n)
        (patho c path)
        (MemState.read_rlxo s s' path l n)

    let read_rlx = ("read_rlx", read_rlxo)

    let write_rlxo c t s c' t' s' =
      fresh (l n path)
        (c  === c')
        (t  === write !!RLX l (const n))
        (t' === skip ())
        (patho c path)
        (MemState.write_rlxo s s' path l n)

    let write_rlx = ("write_rlx", write_rlxo)

    let all = [read_rlx; write_rlx; ]
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
