open MiniKanren
open MiniKanrenStd
open Memory
open Lang
open Expr
open Term
open MemOrder

module Context =
  struct
    module T =
      struct
        type ('ctx, 'tls) t = {
          ctx : 'ctx;
          tls : 'tls
        }

        let fmap fa fb {ctx; tls} =
          { ctx = fa ctx
          ; tls = fb tls
          }
      end

    type tt = (Lang.Context.tt, RegisterStorage.tt) T.t
    type tl = inner MiniKanren.logic
      and inner = (Lang.Context.tl, RegisterStorage.tl) T.t
    type ti = (tt, tl) MiniKanren.injected

    include Fmap2(T)

    let context ctx tls
      inj @@ distrib @@ T.({ctx; tls})

    let tlso ctx tls =
      fresh (ctx')
        (ctx === context ctx' tls)

    let thrdIdo ctx thrdId =
      fresh (ctx' term hole tls)
        (ctx  === context ctx' tls)
        (ctx' === Lang.Context.context term hole thrdId)
  end

type rule =
  Lang.Label.ti -> Context.ti -> Lang.Term.ti -> Lang.Term.ti -> MiniKanren.goal

module Basic =
  struct
    let rec expr_evalo rs e e' = conde [
      fresh (v)
        (e  === const v)
        (e' === e);

      fresh (x v)
        (e  === var x)
        (e' === const v)
        (RegisterStorage.reado rs x v);

      fresh (op l r)
        (e  === binop op l r)
        (e' === const z)
        (expr_evalo rs l (const x))
        (expr_evalo rs r (const y))
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
      fresh (e rs v)
        (t === assertion e)
        (label === Label.empty ())
        (Context.tlso ctx rs)
        (expr_evalo rs e (const v))
        (conde [
          (Lang.Value.nullo v)     &&& (t' === stuck ());
          (Lang.Value.not_nullo v) &&& (t' === skip  ());
        ])

    let asgno label ctx t t' =
      fresh (r e v rs thrdId)
        (t  === asgn r e)
        (t' === skip ())
        (label === Label.regwrite thrdId r v)
        (Context.tlso ctx rs)
        (expr_evalo rs e (const v))
        (Context.thrdIdo ctx thrdId)

    (* let repeato label ctx t t' =
      fresh (e)
        (t  === repeat e)
        (t' === if' e (skip ()) t)
        (label === Label.empty ()) *)

    (* let whileo label ctx t t' =
      fresh (e body)
        (t  === while' e body)
        (t' === if' e (seq body t) t)
        (label === Label.empty ()) *)

    let ifo label ctx t t' =
      fresh (e v rs t1 t2 thrdId)
        (t === if' e t1 t2)
        (label === Label.empty ())
        (Context.tlso ctx rs)
        (expr_evalo rs e (const v))
        (conde [
          (Lang.Value.not_nullo v) &&& (t' === t1);
          (Lang.Value.nullo v)     &&& (t' === t2);
        ])

    let seqo label ctx t t' =
      (t === seq (skip ()) t') &&&
      (label === Label.empty ())

    let all = [asserto; asgno; ifo; seqo]

  end

module ThreadSpawning =
  struct
    let spawno label ctx t t' =
      fresh (t1 t2 thrdId)
       (t  === spw t1 t2)
       (t' === par t1 t2)
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
      fresh (mo l r thrdId)
        (t  === load mo l r)
        (t' === skip ())
        (label === Label.load thrdId mo l r)
        (Context.thrdIdo ctx thrdId)

    let storeo label ctx t t' =
      fresh (mo l e v rs thrdId)
        (t  === store mo l e)
        (t' === skip ())
        (label === Label.store thrdId mo l v)
        (Context.tlso ctx rs)
        (expr_evalo rs e (const v))
        (Context.thrdIdo ctx thrdId)

    let dataraceo label ctx t t' =
      fresh (mo l r e thrdId)
        ((t  === load mo l r) ||| (t === store mo l e))
        (t' === stuck ())
        (label === Label.datarace thrdId mo l)
        (Context.thrdIdo ctx thrdId)

    let all = [loado; storeo; dataraceo]
  end
