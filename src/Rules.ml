open MiniKanren
open MiniKanrenStd
open Memory
open Lang
open MemOrder

open Term
open Context

type ti = Lang.Term.ti
type ci = Lang.Context.ti
type si = Memory.MemState.ti

type rule =  (ci -> ti -> si -> ti -> si -> MiniKanren.goal)

type condition = (ci -> ti -> si -> MiniKanren.goal)

type predicate = (ti * si -> MiniKanrenStd.Bool.groundi -> MiniKanren.goal)

type order = (ti -> ci -> ti -> MiniKanren.goal)

module type CppMemStep = Semantics.StepRelation with
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
  ?(reducibleo = fun (t, s) b -> reducibleo t b)
  rules =
  let stepo (t, s) (t', s') =
    fresh (c rdx rdx')
      (ordero t c rdx)
      (preconditiono c rdx s)
      (conde @@ List.map (fun (name, rule) -> rule c rdx s rdx' s') rules)
      (postconditiono c rdx' s')
      (Context.plugo t' c rdx')
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

    let asgno c t s t' s' =
      fresh (l r n path e)
        (t  === asgn l r)
        (t' === skip ())
        (patho c path)
        (asgno' l r s s' path)

    let asgn = ("assign", asgno)

    let varo c t s t' s' =
      fresh (n x path thrd)
        (s  === s')
        (t  === var x)
        (t' === const n)
        (patho c path)
        (MemState.get_localo s path x n)

    let var = ("var", varo)

    let binopo c t s t' s' = Nat.(
      fresh (op x y z )
        (s  === s')
        (t  === binop op (const x) (const y))
        (t' === const z)
        (conde [
          (op === !!"+")  &&& (Nat.addo x y z);
          (op === !!"*")  &&& (Nat.mulo x y z);
          (op === !!"=")  &&& (conde [(eqo x y !!true ) &&& (z === (inj_nat 1)); (eqo x y !!false) &&& (z === (inj_nat 0))]);
          (op === !!"!=") &&& (conde [(eqo x y !!false) &&& (z === (inj_nat 1)); (eqo x y !!true ) &&& (z === (inj_nat 0))]);
          (op === !!"<")  &&& (conde [(lto x y !!true) &&& (z === (inj_nat 1)); (lto x y !!false) &&& (z === (inj_nat 0))]);
          (op === !!"<=") &&& (conde [(leo x y !!true) &&& (z === (inj_nat 1)); (leo x y !!false) &&& (z === (inj_nat 0))]);
          (op === !!">")  &&& (conde [(gto x y !!true) &&& (z === (inj_nat 1)); (gto x y !!false) &&& (z === (inj_nat 0))]);
          (op === !!">=") &&& (conde [(geo x y !!true) &&& (z === (inj_nat 1)); (geo x y !!false) &&& (z === (inj_nat 0))]);
        ])
      )

    let binop = ("binop", binopo)

    let repeato c t s t' s' =
      fresh (body)
        (s  === s')
        (t  === repeat body)
        (t' === if' body (skip ()) t)

    let repeat = ("repeat", repeato)

    let ifo c t s t' s' =
      fresh (e n btrue bfalse)
        (s === s')
        (t === if' (const n) btrue bfalse)
        (conde [
          fresh (x)
            (n === Nat.succ x) &&& (t' === btrue);

          (n === Nat.zero) &&& (t' === bfalse);
        ])

    let if' = ("if", ifo)

    let seqo c t s t' s' =
      (s === s') &&& (t === seq (skip ()) t')

    let seq = ("seq", seqo)

    let all = [var; binop; asgn; if'; repeat; seq;]

    module Step = (val make_reduction_relation all)

  end

module ThreadSpawning =
  struct

    let spawno c t s t' s' =
      fresh (l r path)
       (t  === spw l r)
       (t' === par l r)
       (patho c path)
       (MemState.spawno s s' path)

    let spawn = ("spawn", spawno)

    let joino c t s t' s' =
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
          (t1 === skip ()) &&& (t2 === skip ()) &&& (t' === skip ());
          (t1 === skip ()) &&& (expro t2) &&& (t' === t2);
          (expro t1) &&& (t2 === skip ()) &&& (t' === t1);
          (expro t1) &&& (expro t2)       &&& (t' === pair t1 t2);
        ])
        (patho c path)
        (MemState.joino s s' path)

    let join = ("join", joino)

    let all = [spawn; join]

    module Step = (val make_reduction_relation all)

  end

module NonAtomic =
  struct
    type ti = Lang.Term.ti
    type ci = Lang.Context.ti
    type si = Memory.MemState.ti

    let read_nao c t s t' s' =
      fresh (l path n ts)
        (t  === read !!NA l)
        (t' === const n)
        (patho c path)
        (MemState.read_nao s s' path l n ts)

    let read_na = ("read_na", read_nao)

    let write_nao c t s t' s' =
      fresh (l n path ts)
        (t  === write !!NA l (const n))
        (t' === skip ())
        (patho c path)
        (MemState.write_nao s s' path l n ts)

    let write_na = ("write_na", write_nao)

    let read_na_dro c t s t' s' =
      fresh (l path n)
        (t  === read !!NA l)
        (t' === stuck ())
        (patho c path)
        (MemState.read_na_dro s s' path l)

    let read_na_dr = ("read_na_dr", read_na_dro)

    let write_na_dro c t s t' s' =
      fresh (l path n ts)
        (t  === write !!NA l (const n))
        (t' === stuck ())
        (patho c path)
        (MemState.write_na_dro s s' path l)

    let write_na_dr = ("write_na_dr", write_na_dro)

    let read_dro c t s t' s' =
      fresh (mo l path n)
        (t  === read mo l)
        (t' === stuck ())
        (patho c path)
        (MemState.read_dro s s' path l)

    let read_dr = ("read_dr", read_dro)

    let write_dro c t s t' s' =
      fresh (mo l path n)
        (t  === write mo l (const n))
        (t' === stuck ())
        (patho c path)
        (MemState.write_dro s s' path l)

    let write_dr = ("write_dr", write_dro)

    let all = [read_na; write_na; read_na_dr; write_na_dr; read_dr; write_dr ]
  end

module Rlx =
  struct

    let read_rlxo c t s t' s' =
      fresh (l path n ts)
        (t  === read !!RLX l)
        (t' === const n)
        (patho c path)
        (MemState.read_rlxo s s' path l n ts)

    let read_rlx = ("read_rlx", read_rlxo)

    let write_rlxo c t s t' s' =
      fresh (l n path ts)
        (t  === write !!RLX l (const n))
        (t' === skip ())
        (patho c path)
        (MemState.write_rlxo s s' path l n ts)

    let write_rlx = ("write_rlx", write_rlxo)

    let all = [read_rlx; write_rlx; ]

    module Step = (val make_reduction_relation all)
  end

module RelAcq =
  struct

    let read_acqo c t s t' s' =
      fresh (l path n ts)
        (t  === read !!ACQ l)
        (t' === const n)
        (patho c path)
        (MemState.read_acqo s s' path l n ts)

    let read_acq = ("read_acq", read_acqo)

    let write_relo c t s t' s' =
      fresh (l n path ts)
        (t  === write !!REL l (const n))
        (t' === skip ())
        (patho c path)
        (MemState.write_relo s s' path l n ts)

    let write_rel = ("write_rel", write_relo)

    let all = [read_acq; write_rel;]

    module Step = (val make_reduction_relation all)
  end

module SC =
  struct

    let read_sco c t s t' s' =
      fresh (l path n ts)
        (t  === read !!SC l)
        (t' === const n)
        (patho c path)
        (MemState.read_sco s s' path l n ts)

    let read_sc = ("read_sc", read_sco)

    let write_sco c t s t' s' =
      fresh (l n path ts)
        (t  === write !!SC l (const n))
        (t' === skip ())
        (patho c path)
        (MemState.write_sco s s' path l n ts)

    let write_sc = ("write_sc", write_sco)

    let all = [read_sc; write_sc;]


  end

let certifyo rules path t s  =
  let preconditiono c _ _ = Context.patho c path in
  let reducibleo = fun (t, _) b -> reducibleo ~path t b in
  let module CertStep =
    (val make_reduction_relation ~preconditiono ~reducibleo rules)
  in
  let module Cert = Semantics.Make(CertStep) in
  Cert.(
    fresh (t' s')
      (Memory.MemState.certifyo s' path)
      ((t, s) -->* (t', s'))
  )

module Promising =
  struct

    let promiseo c t s t' s' =
      fresh (l n path)
        (t  === write !!RLX l (const n))
        (t' === skip ())
        (patho c path)
        (MemState.promiseo s s' (Path.pathl @@ Path.pathn ()) l n)

    let promise = ("promise", promiseo)

    let fulfillo c t s t' s' =
      fresh (path)
        (t  === t')
        (patho c path)
        (MemState.fulfillo s s' path)

    let fulfill = ("fulfill", fulfillo)

    let all = [promise; fulfill]

    module PromiseStep =
      (val make_reduction_relation
        ~reducibleo:(fun (t, _) b -> can_prmo t b)
        ~ordero:pick_prmo
        [promise]
      )

    module FulfillStep =
      (val make_reduction_relation
        ~reducibleo:(fun (_, s) b -> MemState.laggingo s b)
        ~ordero:Context.dumb_splito
        [fulfill]
      )

    let make_certified_step rules =
      let ext_rules = fulfill::rules in
      let reducibleo (t, s) b =
        fresh (b1 b2)
          (Term.reducibleo t b1)
          (MemState.laggingo s b2)
          (Bool.oro b1 b2 b)
      in
      let postconditiono c rdx s =
        fresh (t path)
          (patho c path)
          (plugo t c rdx)
          (certifyo ext_rules path t s)
      in
      let module CertStep =
        (val make_reduction_relation (*~postconditiono*) ~reducibleo ext_rules) in
        (module struct
          include Semantics.UnionRelation(CertStep)(PromiseStep)
          (* include CertStep *)
          (* include PromiseStep *)
        end : CppMemStep)

  end
