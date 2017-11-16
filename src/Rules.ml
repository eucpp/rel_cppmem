open MiniKanren
open MiniKanrenStd
open Memory
open Lang
open Expr
open Term
open MemOrder

type rule =
  Lang.Label.ti -> Lang.Context.ti -> Lang.Term.ti -> Lang.Term.ti -> MiniKanren.goal

module Basic =
  struct
    let rec expr_evalo s e e' = conde [
      fresh (v)
        (e  === const v)
        (e' === e);

      fresh (x v)
        (e  === var x)
        (e' === const v)
        (RegisterStorage.reado s x v);

      fresh (op l r)
        (e  === binop op l r)
        (e' === const z)
        (expr_evalo s l (const x))
        (expr_evalo s r (const y))
        (conde [
          (op === !!Op.ADD) &&& (addo x y z);
          (op === !!Op.MUL) &&& (mulo x y z);
          (op === !!Op.EQ ) &&& (conde [(eqo x y) &&& (z === (integer 1)); (nqo x y) &&& (z === (integer 0))]);
          (op === !!Op.NEQ) &&& (conde [(nqo x y) &&& (z === (integer 1)); (eqo x y) &&& (z === (integer 0))]);
          (op === !!Op.LT ) &&& (conde [(lto x y) &&& (z === (integer 1)); (geo x y) &&& (z === (integer 0))]);
          (op === !!Op.LE ) &&& (conde [(leo x y) &&& (z === (integer 1)); (gto x y) &&& (z === (integer 0))]);
          (op === !!Op.GT ) &&& (conde [(gto x y) &&& (z === (integer 1)); (leo x y) &&& (z === (integer 0))]);
          (op === !!Op.GE ) &&& (conde [(geo x y) &&& (z === (integer 1)); (lto x y) &&& (z === (integer 0))]);

          (op === !!Lang.Op.OR ) &&& (conde [
            (nullo x)     &&& (nullo y)     &&& (z === (integer 0));
            (not_nullo x) &&& (nullo y)     &&& (z === (integer 1));
            (nullo x)     &&& (not_nullo y) &&& (z === (integer 1));
            (not_nullo x) &&& (not_nullo y) &&& (z === (integer 1));
          ]);

          (op === !!Lang.Op.AND) &&& (conde [
            (nullo x)     &&& (nullo y)     &&& (z === (integer 0));
            (not_nullo x) &&& (nullo y)     &&& (z === (integer 0));
            (nullo x)     &&& (not_nullo y) &&& (z === (integer 0));
            (not_nullo x) &&& (not_nullo y) &&& (z === (integer 1));
          ]);
        ])
    ]

    let asserto label ctx t t' =
      fresh (v)
        (t === assertion (const v))
        (conde [
          (Lang.Value.nullo v)     &&& (t' === stuck ());
          (Lang.Value.not_nullo v) &&& (t' === skip  ());
        ])
        (label === Label.empty ())

    let asgno label ctx t t' =
      fresh (r e thrdId)
        (t  === asgn r e)
        (t' === skip ())
        (label === Label.regwrite thrdId r e)
        (Context.thrdIdo ctx thrdId)

    (* let repeato label ctx t t' =
      fresh (e)
        (t  === repeat e)
        (t' === if' e (skip ()) t)
        (label === Label.empty ()) *)

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

    let all = [asserto; asgno; ifo; whileo; seqo]

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
      fresh (t1 t2 thrdId)
        (t === par t1 t2)
        (conde [
          (t1 =/= stuck ()) &&& (t2 =/= stuck ()) &&& (t' === skip ())  &&& (label === Label.join thrdId);
          (t1 =/= stuck ()) &&& (t2 === stuck ()) &&& (t' === stuck ()) &&& (label === Label.empty ());
          (t1 === stuck ()) &&& (t' === stuck ()) &&& (label === Label.empty ());
        ])
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
