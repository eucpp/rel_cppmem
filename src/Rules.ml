open MiniKanren
open MiniKanrenStd
open Memory
open Lang
open Term
open MemOrder

type rule =
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
        (label === Label.empty ())
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
        (label === Label.empty ())

    let whileo label ctx t t' =
      fresh (e body)
        (t  === while' e body)
        (t' === if' e (seq body t) t)
        (label === Label.empty ())

    let ifo label ctx t t' =
      fresh (e x t1 t2 thrdId)
        (t === if' (const x) t1 t2)
        (conde [
          (Lang.Value.not_nullo x) &&& (t' === t1);
          (Lang.Value.nullo x)     &&& (t' === t2);
        ])
        (label === Label.empty ())

    let seqo label ctx t t' =
      (t  === seq (skip ()) t') &&&
      (label === Label.empty ())

    let asserto label ctx t t' =
      fresh (v)
        (t === assertion (const v))
        (conde [
          (Lang.Value.nullo v)     &&& (t' === skip ());
          (Lang.Value.not_nullo v) &&& (t' === stuck ());
        ])
        (label === Label.empty ())

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
      fresh (mo l n thrdId)
        (t  === read mo l)
        (t' === const n)
        (label === Label.load thrdId mo l n)
        (Context.thrdIdo ctx thrdId)

    let storeo label ctx t t' =
      fresh (mo l n thrdId)
        (t  === write mo l (const n))
        (t' === skip ())
        (label === Label.store thrdId mo l n)
        (Context.thrdIdo ctx thrdId)

    let dataraceo label ctx t t' =
      fresh (mo l n thrdId)
        ((t  === read mo l) ||| (t === write mo l (const n)))
        (t' === stuck ())
        (label === Label.datarace thrdId mo l)
        (Context.thrdIdo ctx thrdId)

    let all = [loado; storeo; dataraceo;]
  end
