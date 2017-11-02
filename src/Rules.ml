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
    include Fmap(T)

    type tt = Lang.ThreadID.tt t
    type tl = Lang.ThreadID.tl t MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    let constraints ~thrdId =
      MiniKanren.inj @@ distrib @@ { thrdId }

    let thrd_ido t thrdId =
      (t === constraints ~thrdId)
  end

let check_thrdo ctrs ctx thrdId =
  (* TODO: we really should check that thrdId in constraints is a parent of the context's thrdId *)
  (Constraints.thrd_ido ctrs thrdId) &&& (Lang.Context.thrdIdo ctx thrdId)

module RuleTypes (Machine : Machines.Sequential) =
  struct
    module CFG = Semantics.Configuration(Lang.Term)(Machine)

    type tt = CFG.tt
    type tl = CFG.tl

    type ct = Lang.Context.tt
    type cl = Lang.Context.tl

    type cst = Constraints.tt
    type csl = Constraints.tl

    type rule = (tt, ct, cst, tl, cl, csl) Semantics.rule
  end

module Basic (Machine : Machines.Sequential) =
  struct
    include RuleTypes(Machine)

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

    let asgno = CFG.lift_rule asgno

    let varo ctrs ctx t s t' s' =
      fresh (n x thrdId)
        (s  === s')
        (t  === var x)
        (t' === const n)
        (check_thrdo ctrs ctx thrdId)
        (Machine.reado s thrdId x n)

    let varo = CFG.lift_rule varo

    let binopo ctrs ctx t s t' s' = Lang.(Value.(
      fresh (op x y z thrdId)
        (t  === binop op (const x) (const y))
        (t' === const z)
        (s  === s')
        (check_thrdo ctrs ctx thrdId)
        (conde [
          (op === !!Lang.Op.ADD) &&& (addo x y z);
          (op === !!Lang.Op.MUL) &&& (mulo x y z);
          (op === !!Lang.Op.EQ ) &&& (conde [(eqo x y !!true ) &&& (z === (integer 1)); (eqo x y !!false) &&& (z === (integer 0))]);
          (op === !!Lang.Op.NEQ) &&& (conde [(eqo x y !!false) &&& (z === (integer 1)); (eqo x y !!true ) &&& (z === (integer 0))]);
          (op === !!Lang.Op.LT ) &&& (conde [(lto x y !!true) &&& (z === (integer 1)); (lto x y !!false) &&& (z === (integer 0))]);
          (op === !!Lang.Op.LE ) &&& (conde [(leo x y !!true) &&& (z === (integer 1)); (leo x y !!false) &&& (z === (integer 0))]);
          (op === !!Lang.Op.GT ) &&& (conde [(gto x y !!true) &&& (z === (integer 1)); (gto x y !!false) &&& (z === (integer 0))]);
          (op === !!Lang.Op.GE ) &&& (conde [(geo x y !!true) &&& (z === (integer 1)); (geo x y !!false) &&& (z === (integer 0))]);
        ])
      ))

    let binopo = CFG.lift_rule binopo

    let repeato ctrs ctx t s t' s' =
      fresh (e thrdId)
        (t  === repeat e)
        (t' === if' e (skip ()) t)
        (s  === s')
        (check_thrdo ctrs ctx thrdId)

    let repeato = CFG.lift_rule repeato

    let whileo ctrs ctx t s t' s' =
      fresh (e body thrdId)
        (t  === while' e body)
        (t' === if' e (seq body t) t)
        (s  === s')
        (check_thrdo ctrs ctx thrdId)

    let whileo = CFG.lift_rule whileo

    let ifo ctrs ctx t s t' s' =
      fresh (e x t1 t2 thrdId)
        (t === if' (const x) t1 t2)
        (conde [
          (Lang.Value.not_nullo x) &&& (t' === t1);
          (Lang.Value.nullo x)     &&& (t' === t2);
        ])
        (s === s')
        (check_thrdo ctrs ctx thrdId)

    let ifo = CFG.lift_rule ifo

    let seqo ctrs ctx t s t' s' =
      fresh (thrdId)
        (t  === seq (skip ()) t')
        (s  === s')
        (check_thrdo ctrs ctx thrdId)

    let seqo = CFG.lift_rule seqo

    let asserto ctrs ctx t s t' s' =
      fresh (v thrdId)
        (t === assertion (const v))
        (conde [
          (Lang.Value.nullo v)     &&& (t' === skip ());
          (Lang.Value.not_nullo v) &&& (t' === stuck ());
        ])
        (s === s')
        (check_thrdo ctrs ctx thrdId)

    let asserto = CFG.lift_rule asserto

    let all = [varo; binopo; asgno; ifo; repeato; seqo; asserto]

  end

module ThreadSpawning (Machine : Machines.Parallel) =
  struct
    include RuleTypes(Machine)

    let spawno ctrs ctx t s t' s' =
      fresh (l r thrdId)
       (t  === spw l r)
       (t' === par l r)
       (check_thrdo ctrs ctx thrdId)
       (Machine.spawno s s' thrdId)

    let spawno = CFG.lift_rule spawno

    let joino ctrs ctx t s t' s' =
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
        (check_thrdo ctrs ctx thrdId)
        (Machine.joino s s' thrdId)

    let joino = CFG.lift_rule joino

    let all = [spawno; joino]
  end


module NonAtomic (Machine : Machines.NonAtomic) =
  struct
    include RuleTypes(Machine)

    let load_nao ctrs ctx t s t' s' =
      fresh (l thrdId n)
        (t  === read !!NA l)
        (t' === const n)
        (check_thrdo ctrs ctx thrdId)
        (Machine.load_nao s s' thrdId l n)

    let load_nao = CFG.lift_rule load_nao

    let store_nao ctrs ctx t s t' s' =
      fresh (l n thrdId)
        (t  === write !!NA l (const n))
        (t' === skip ())
        (check_thrdo ctrs ctx thrdId)
        (Machine.store_nao s s' thrdId l n)

    let store_nao = CFG.lift_rule store_nao

    let load_data_raceo ctrs ctx t s t' s' =
      fresh (mo l n thrdId)
        (t  === read mo l)
        (t' === stuck ())
        (check_thrdo ctrs ctx thrdId)
        (Machine.load_data_raceo s s' thrdId mo l)

    let load_data_raceo = CFG.lift_rule load_data_raceo

    let store_data_raceo ctrs ctx t s t' s' =
      fresh (mo l n thrdId)
        (t  === write mo l (const n))
        (t' === stuck ())
        (check_thrdo ctrs ctx thrdId)
        (Machine.store_data_raceo s s' thrdId mo l)

    let store_data_raceo = CFG.lift_rule store_data_raceo

    let all = [load_nao; store_nao; load_data_raceo; store_data_raceo]
  end

module SequentialConsistent (Machine : Machines.SequentialConsistent) =
  struct
    include RuleTypes(Machine)

    let load_sco ctrs ctx t s t' s' =
      fresh (l thrdId n)
        (t  === read !!SC l)
        (t' === const n)
        (check_thrdo ctrs ctx thrdId)
        (Machine.load_sco s s' thrdId l n)

    let load_sco = CFG.lift_rule load_sco

    let store_sco ctrs ctx t s t' s' =
      fresh (l n thrdId)
        (t  === write !!SC l (const n))
        (t' === skip ())
        (check_thrdo ctrs ctx thrdId)
        (Machine.store_sco s s' thrdId l n)

    let store_sco = CFG.lift_rule store_sco

    let cas_sco ctrs ctx t s t' s' =
      fresh (l e d v thrdId)
        (t  === cas !!SC !!SC l (const e) (const d))
        (t' === const v)
        (check_thrdo ctrs ctx thrdId)
        (Machine.cas_sco s s' thrdId l e d v)

    let cas_sco = CFG.lift_rule cas_sco

    let all = [load_sco; store_sco; cas_sco;]
  end

module ReleaseAcquire (Machine : Machines.ReleaseAcquire) =
  struct
    include RuleTypes(Machine)

    let load_acqo ctrs ctx t s t' s' =
      fresh (l thrdId n)
        (t  === read !!ACQ l)
        (t' === const n)
        (check_thrdo ctrs ctx thrdId)
        (Machine.load_acqo s s' thrdId l n)

    let load_acqo = CFG.lift_rule load_acqo

    let store_relo ctrs ctx t s t' s' =
      fresh (l n thrdId)
        (t  === write !!REL l (const n))
        (t' === skip ())
        (check_thrdo ctrs ctx thrdId)
        (Machine.store_relo s s' thrdId l n)

    let store_relo = CFG.lift_rule store_relo

    let all = [load_acqo; store_relo;]
  end

(*
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
