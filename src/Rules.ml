open MiniKanren
open Memory
open Lang
open MemOrder

open Term
open Context

type ti = Lang.Term.ti
type ci = Lang.Context.ti
type si = Memory.MemState.ti

type rule =  (ci -> ti -> si -> ci -> ti -> si -> MiniKanren.goal)

type condition = (ci -> ti -> si -> MiniKanren.goal)

type predicate = (ti -> MiniKanren.Bool.groundi -> MiniKanren.goal)

type order = (ti -> ci -> ti -> MiniKanren.goal)

module type CppMemStep = Semantics.Step with
  type tt = Lang.Term.tt       and
  type tl = Lang.Term.tl       and
  type st = Memory.MemState.tt and
  type sl = Memory.MemState.tl

let make_step :
  ?reducibleo:(Term.ti * Memory.MemState.ti -> Bool.groundi -> MiniKanren.goal) ->
  stepo:(Term.ti * Memory.MemState.ti -> Term.ti * Memory.MemState.ti -> MiniKanren.goal) ->
  (module CppMemStep) =
  fun ?(reducibleo = fun (t, s) b -> reducibleo t b) ~stepo -> (module
    struct
      type tt = Term.tt
      type tl = Term.tl
      type ti = (tt, tl) MiniKanren.injected

      type st = Memory.MemState.tt
      type sl = Memory.MemState.tl
      type si = (st, sl) MiniKanren.injected

      let (->?) = reducibleo
      let (-->) = stepo

    end : CppMemStep)

let make_reduction_relation
  ?(preconditiono  = fun _ _ _ -> success)
  ?(postconditiono = fun _ _ _ -> success)
  ?(ordero = splito)
  ?reducibleo
  rules =
  let stepo (t, s) (t', s') =
    fresh (c c' rdx rdx')
      (ordero t c rdx)
      (preconditiono c rdx s)
      (conde @@ List.map (fun (name, rule) -> rule c rdx s c' rdx' s') rules)
      (postconditiono c' rdx' s')
      (Context.plugo t' c' rdx')
  in
  make_step ~reducibleo ~stepo

module Basic =
  struct

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

    let module Step = (val make_reduction_relation all)

  end

module ThreadSpawning =
  struct

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

    let module Step = (val make_reduction_relation all)

  end

module Rlx =
  struct

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

    let module Step = (val make_reduction_relation all)

  end

module RelAcq =
  struct

    let read_acqo c t s c' t' s' =
      fresh (l path n)
        (c  === c')
        (t  === read !!ACQ l)
        (t' === const n)
        (patho c path)
        (MemState.read_acqo s s' path l n)

    let read_acq = ("read_acq", read_acqo)

    let write_relo c t s c' t' s' =
      fresh (l n path)
        (c  === c')
        (t  === write !!REL l (const n))
        (t' === skip ())
        (patho c path)
        (MemState.write_relo s s' path l n)

    let write_rel = ("write_rel", write_relo)

    let all = [read_acq; write_rel; ]

    let module Step = (val make_reduction_relation all)

  end

let certifyo path t s rules =
  let module CertStep =
    let precondition c _ _ = Context.patho c path in
    let reducibleo = reducibleo ~path in
    make_reduction_relation ~precondition ~reducibleo rules
  in
  let module Cert = Semantics.Make(CertStep) in
  Cert.(
    fresh (t' s')
      (Memory.MemState.certifyo s' path)
      ((t, s) -->* (t', s'))
  )


module Promising =
  struct

    let promiseo c t s c' t' s' =
      fresh (l n mo path)
        (c  === c')
        (t  === write !!RLX l (const n))
        (t' === skip ())
        (patho c path)
        (MemState.promiseo s s' path l n)

    let promise = ("promise", promiseo)

    let fulfillo c t s c' t' s' =
      fresh (path)
        (c  === c')
        (t  === t')
        (patho c path)
        (MemState.fulfillo s s' path)

    let fulfill = ("fulfill", fulfillo)

    let all = [promise; fulfill]

    let reducibleo (t, s) b = conde [
      can_prmo t b;
      
    ]

    let module Step = (val make_reduction_relation all)

  end
