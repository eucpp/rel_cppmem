open MiniKanren
open MiniKanrenStd
open Memory
open Lang
open Term
open MemOrder

module Constraints =
  struct
    module T =
      struct
        type ('thrdId) t = {
          thrdId : 'thrdId;
        }

        let fmap f { thrdId } = { thrdId = f thrdId }
      end

    include T
    include Fmap1(T)

    type tt = Lang.ThreadID.tt t
    type tl = Lang.ThreadID.tl t MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    let constraints ~thrdId =
      MiniKanren.inj @@ distrib @@ { thrdId }

    let thrd_ido t thrdId =
      (t === constraints ~thrdId)
  end

module Basic (Machine : Machines.Sequential) =
  struct
    type tt = (Lang.Term.tt, Machine.tt) Semantics.Configuration.tt
    type tl = (Lang.Term.tl, Machine.tl) Semantics.Configuration.tl

    type ct = Lang.Context.tt
    type cl = Lang.Context.tl

    type cst = Constraints.tt
    type csl = Constraints.tl

    type rule = (tt, ct, cst, tl, cl, csl) Semantics.rule

    let check_thrdo ctrs ctx thrdId =
      (* TODO: we really should check that thrdId in constraints is a parent of the context's thrdId *)
      (Constraints.thrd_ido ctrs thrdId) &&& (Lang.Context.thrdIdo ctx thrdId)

    let rec asgno' l r s s' thrdId = conde [
      fresh (x n)
        (l === var x)
        (r === const n)
        (Machine.writeo s s' thrdId x n);
      fresh (x1 t n1 e s'')
        (l === pair (var   x1) t)
        (r === pair (const n1) e)
        (Machine.writeo s  s'' thrdId x1 n1)
        (asgno' t e s'' s' thrdId);
    ]

    let asgno ctrs ctx t s t' s' =
      fresh (l r n thrdId e)
        (t  === asgn l r)
        (t' === skip ())
        (check_thrdo ctrs ctx thrdId)
        (asgno' l r s s' thrdId)

    let asgno = Semantics.Configuration.lift_rule asgno

    let varo ctrs ctx t s t' s' =
      fresh (n x thrdId)
        (s  === s')
        (t  === var x)
        (t' === const n)
        (check_thrdo ctrs ctx thrdId)
        (Machine.reado s thrdId x n)

    let varo = Semantics.Configuration.lift_rule varo

    let binopo ctrs ctx t s t' s' = Lang.(Value.(
      fresh (op x y z thrdId)
        (t  === binop op (const x) (const y))
        (t' === const z)
        (s  === s')
        (check_thrdo ctrs ctx thrdId)
        (conde [
          (op === !!Lang.Op.ADD) &&& (addo x y z);
          (op === !!Lang.Op.MUL) &&& (mulo x y z);
          (op === !!Lang.Op.EQ ) &&& (conde [(eqo x y !!true ) &&& (z === (value 1)); (eqo x y !!false) &&& (z === (value 0))]);
          (op === !!Lang.Op.NEQ) &&& (conde [(eqo x y !!false) &&& (z === (value 1)); (eqo x y !!true ) &&& (z === (value 0))]);
          (op === !!Lang.Op.LT ) &&& (conde [(lto x y !!true) &&& (z === (value 1)); (lto x y !!false) &&& (z === (value 0))]);
          (op === !!Lang.Op.LE ) &&& (conde [(leo x y !!true) &&& (z === (value 1)); (leo x y !!false) &&& (z === (value 0))]);
          (op === !!Lang.Op.GT ) &&& (conde [(gto x y !!true) &&& (z === (value 1)); (gto x y !!false) &&& (z === (value 0))]);
          (op === !!Lang.Op.GE ) &&& (conde [(geo x y !!true) &&& (z === (value 1)); (geo x y !!false) &&& (z === (value 0))]);
        ])
      ))

    let binopo = Semantics.Configuration.lift_rule binopo

    let repeato ctrs ctx t s t' s' =
      fresh (body thrdId)
        (t  === repeat body)
        (t' === if' body (skip ()) t)
        (s  === s')
        (check_thrdo ctrs ctx thrdId)

    let repeato = Semantics.Configuration.lift_rule repeato

    let ifo ctrs ctx t s t' s' =
      fresh (e n btrue bfalse thrdId)
        (t === if' (const n) btrue bfalse)
        (conde [
          fresh (x)
            (n === Lang.Value.succ x) &&& (t' === btrue);
          (n === Lang.Value.zero ()) &&& (t' === bfalse);
        ])
        (s  === s')
        (check_thrdo ctrs ctx thrdId)

    let ifo = Semantics.Configuration.lift_rule ifo

    let seqo ctrs ctx t s t' s' =
      fresh (thrdId)
        (t  === seq (skip ()) t')
        (s  === s')
        (check_thrdo ctrs ctx thrdId)

    let seqo = Semantics.Configuration.lift_rule seqo

    let all = [varo; binopo; asgno; ifo; repeato; seqo;]

  end

(*
module ThreadSpawning =
  struct

    let spawno c t s t' s' =
      fresh (l r thrdId)
       (t  === spw l r)
       (t' === par l r)
       (thrdIdo c thrdId)
       (MemState.spawno s s' thrdId)

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
      fresh (t1 t2 thrdId)
        (t === par t1 t2)
        (conde [
          (t1 === skip ()) &&& (t2 === skip ()) &&& (t' === skip ());
          (t1 === skip ()) &&& (expro t2) &&& (t' === t2);
          (expro t1) &&& (t2 === skip ()) &&& (t' === t1);
          (expro t1) &&& (expro t2)       &&& (t' === pair t1 t2);
        ])
        (thrdIdo c thrdId)
        (MemState.joino s s' thrdId)

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
      fresh (l thrdId n ts)
        (t  === read !!NA l)
        (t' === const n)
        (thrdIdo c thrdId)
        (MemState.read_nao s s' thrdId l n ts)

    let read_na = ("read_na", read_nao)

    let write_nao c t s t' s' =
      fresh (l n thrdId ts)
        (t  === write !!NA l (const n))
        (t' === skip ())
        (thrdIdo c thrdId)
        (MemState.write_nao s s' thrdId l n ts)

    let write_na = ("write_na", write_nao)

    let read_na_dro c t s t' s' =
      fresh (l thrdId n)
        (t  === read !!NA l)
        (t' === stuck ())
        (thrdIdo c thrdId)
        (MemState.read_na_dro s s' thrdId l)

    let read_na_dr = ("read_na_dr", read_na_dro)

    let write_na_dro c t s t' s' =
      fresh (l thrdId n ts)
        (t  === write !!NA l (const n))
        (t' === stuck ())
        (thrdIdo c thrdId)
        (MemState.write_na_dro s s' thrdId l)

    let write_na_dr = ("write_na_dr", write_na_dro)

    let read_dro c t s t' s' =
      fresh (mo l thrdId n)
        (t  === read mo l)
        (t' === stuck ())
        (thrdIdo c thrdId)
        (MemState.read_dro s s' thrdId l)

    let read_dr = ("read_dr", read_dro)

    let write_dro c t s t' s' =
      fresh (mo l thrdId n)
        (t  === write mo l (const n))
        (t' === stuck ())
        (thrdIdo c thrdId)
        (MemState.write_dro s s' thrdId l)

    let write_dr = ("write_dr", write_dro)

    let all = [read_na; write_na; read_na_dr; write_na_dr; read_dr; write_dr ]
  end

module Rlx =
  struct

    let read_rlxo c t s t' s' =
      fresh (l thrdId n ts)
        (t  === read !!RLX l)
        (t' === const n)
        (thrdIdo c thrdId)
        (MemState.read_rlxo s s' thrdId l n ts)

    let read_rlx = ("read_rlx", read_rlxo)

    let write_rlxo c t s t' s' =
      fresh (l n thrdId ts)
        (t  === write !!RLX l (const n))
        (t' === skip ())
        (thrdIdo c thrdId)
        (MemState.write_rlxo s s' thrdId l n ts)

    let write_rlx = ("write_rlx", write_rlxo)

    let all = [read_rlx; write_rlx; ]

    module Step = (val make_reduction_relation all)
  end

module RelAcq =
  struct

    let read_acqo c t s t' s' =
      fresh (l thrdId n ts)
        (t  === read !!ACQ l)
        (t' === const n)
        (thrdIdo c thrdId)
        (MemState.read_acqo s s' thrdId l n ts)

    let read_acq = ("read_acq", read_acqo)

    let write_relo c t s t' s' =
      fresh (l n thrdId ts)
        (t  === write !!REL l (const n))
        (t' === skip ())
        (thrdIdo c thrdId)
        (MemState.write_relo s s' thrdId l n ts)

    let write_rel = ("write_rel", write_relo)

    let all = [read_acq; write_rel;]

    module Step = (val make_reduction_relation all)
  end

module SC =
  struct

    let read_sco c t s t' s' =
      fresh (l thrdId n ts)
        (t  === read !!SC l)
        (t' === const n)
        (thrdIdo c thrdId)
        (MemState.read_sco s s' thrdId l n ts)

    let read_sc = ("read_sc", read_sco)

    let write_sco c t s t' s' =
      fresh (l n thrdId ts)
        (t  === write !!SC l (const n))
        (t' === skip ())
        (thrdIdo c thrdId)
        (MemState.write_sco s s' thrdId l n ts)

    let write_sc = ("write_sc", write_sco)

    let all = [read_sc; write_sc;]


  end

let certifyo rules thrdId t s  =
  let preconditiono c _ _ = Context.thrdIdo c thrdId in
  (* let reducibleo = fun (t, _) b -> reducibleo ~thrdId t b in *)
  let module CertStep =
    (val make_reduction_relation ~preconditiono (*~reducibleo*) rules)
  in
  let module Cert = Semantics.Make(CertStep) in
  Cert.(
    fresh (t' s')
      (Memory.MemState.certifyo s' thrdId)
      ((t, s) -->* (t', s'))
  )

module Promising =
  struct

    let promiseo c t s t' s' =
      fresh (l n thrdId)
        (t  === write !!RLX l (const n))
        (t' === skip ())
        (thrdIdo c thrdId)
        (MemState.promiseo s s' (Path.thrdIdl @@ Path.thrdIdn ()) l n)

    let promise = ("promise", promiseo)

    let fulfillo c t s t' s' =
      fresh (thrdId)
        (t  === t')
        (thrdIdo c thrdId)
        (MemState.fulfillo s s' thrdId)

    let fulfill = ("fulfill", fulfillo)

    let all = [promise; fulfill]

    module PromiseStep =
      (val make_reduction_relation
        ~ordero:Lang.promiseo
        [promise]
      )

    module FulfillStep =
      (val make_reduction_relation
        (* ~reducibleo:(fun (_, s) b -> MemState.laggingo s b) *)
        (* ~ordero:Context.dumb_splito *)

        [fulfill]
      )

    let make_certified_step rules =
      let ext_rules = fulfill::rules in
      (* let reducibleo (t, s) b =
        fresh (b1 b2)
          (Term.reducibleo t b1)
          (MemState.laggingo s b2)
          (Bool.oro b1 b2 b)
      in *)
      let postconditiono c rdx s =
        fresh (t thrdId)
          (thrdIdo c thrdId)
          (plugo c rdx t)
          (certifyo ext_rules thrdId t s)
      in
      let module CertStep =
        (val make_reduction_relation (*~postconditiono*) (*~reducibleo*) ext_rules) in
        (module struct
          include Semantics.UnionRelation(CertStep)(PromiseStep)
          (* include CertStep *)
          (* include PromiseStep *)
        end : CppMemStep)

  end
*)
