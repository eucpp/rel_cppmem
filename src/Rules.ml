open MiniKanren
open MiniKanrenStd
open Memory
open Lang
open Term
open MemOrder

type rule :
  Lang.Label.ti -> Lang.Context.ti -> Lang.Term.ti -> Lang.Term.ti -> MiniKanren.goal

module Basic =
  struct
    let asgno label ctx t t' =
      fresh (x n thrdId)
        (t  === asgn (var x) (const n))
        (t' === skip ())
        (label === Label.regwrite thrdId x n)
        (Context.thrdIdo ctx thrdId)

    let varo label ctx t t' =
      fresh (n x thrdId)
        (t  === var x)
        (t' === const n)
        (label === Label.regread thrdId x n)
        (Context.thrdIdo ctx thrdId)

    let binopo label ctx t t' = Lang.(Value.(
      fresh (op x y z)
        (t  === binop op (const x) (const y))
        (t' === const z)
        (label === Label.none ())
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

    let repeato label ctx t t' =
      fresh (e)
        (t  === repeat e)
        (t' === if' e (skip ()) t)
        (label === Label.none ())

    let whileo label ctx t t' =
      fresh (e body)
        (t  === while' e body)
        (t' === if' e (seq body t) t)
        (label === Label.none ())

    let ifo label ctx t t' =
      fresh (e x t1 t2 thrdId)
        (t === if' (const x) t1 t2)
        (conde [
          (Lang.Value.not_nullo x) &&& (t' === t1);
          (Lang.Value.nullo x)     &&& (t' === t2);
        ])
        (label === Label.none ())

    let seqo ctrs ctx t s t' s' =
      fresh (thrdId)
        (t  === seq (skip ()) t')
        (s  === s')
        (check_thrdo ctrs ctx thrdId)

    let asserto label ctx t t' =
      fresh (v)
        (t === assertion (const v))
        (conde [
          (Lang.Value.nullo v)     &&& (t' === skip ());
          (Lang.Value.not_nullo v) &&& (t' === stuck ());
        ])
        (label === Label.none ())

    let all = [varo; binopo; asgno; ifo; whileo; repeato; seqo; asserto]

  end

module ThreadSpawning =
  struct
    let spawno label ctx t t' =
      fresh (l r thrdId)
       (t  === spw l r)
       (t' === par l r)
       (label === Label.spawn thrdId)
       (Context.thrdIdo ctx thrdId)

    let joino label ctx t t' =
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
        (label === Label.join thrdId)
        (Context.thrdIdo ctx thrdId)

    let all = [spawno; joino]
  end


module Atomic =
  struct
    let loado label ctx t t' =
      fresh (mo n l thrdId)
        (t  === read mo l)
        (t' === const n)
        (label === Label.load thrdId l n)
        (Context.thrdIdo ctx thrdId)

    let storeo label ctx t t' =
      fresh (l n thrdId)
        (t  === write !!NA l (const n))
        (t' === skip ())
        (check_thrdo ctrs ctx thrdId)
        (Machine.store_nao s s' thrdId l n)

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
