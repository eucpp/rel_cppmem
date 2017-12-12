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

    let context ctx tls =
      inj @@ distrib @@ T.({ctx; tls})

    let hole h =
      inj @@ distrib @@ T.({ ctx = Lang.Context.hole h; tls = RegisterStorage.empty () })

    let regso ctx tls =
      fresh (ctx')
        (ctx === context ctx' tls)

    let thrdIdo ctx thrdId =
      fresh (ctx' tls)
        (ctx  === context ctx' tls)
        (Lang.Context.thrdIdo ctx' thrdId)
  end

type rule =
  Lang.Label.ti -> Context.ti -> Context.ti -> Lang.Term.ti -> Lang.Term.ti -> MiniKanren.goal

let rec expr_evalo rs e v = Value.(conde [
  (e === const v);

  fresh (x)
    (e === var x)
    (RegisterStorage.reado rs x v);

  fresh (op e' v')
    (e === unop !!Uop.NOT e')
    (conde [
      (Value.nullo v')     &&& (v === (integer 1));
      (Value.not_nullo v') &&& (v === (integer 0));
    ])
    (expr_evalo rs e' v');

  fresh (op l r x y)
    (e  === binop op l r)
    (expr_evalo rs l x)
    (expr_evalo rs r y)
    (conde [
      (op === !!Bop.ADD) &&& (addo x y v);
      (op === !!Bop.MUL) &&& (mulo x y v);
      (op === !!Bop.EQ ) &&& (conde [(eqo x y) &&& (v === (integer 1)); (nqo x y) &&& (v === (integer 0))]);
      (op === !!Bop.NEQ) &&& (conde [(nqo x y) &&& (v === (integer 1)); (eqo x y) &&& (v === (integer 0))]);
      (op === !!Bop.LT ) &&& (conde [(lto x y) &&& (v === (integer 1)); (geo x y) &&& (v === (integer 0))]);
      (op === !!Bop.LE ) &&& (conde [(leo x y) &&& (v === (integer 1)); (gto x y) &&& (v === (integer 0))]);
      (op === !!Bop.GT ) &&& (conde [(gto x y) &&& (v === (integer 1)); (leo x y) &&& (v === (integer 0))]);
      (op === !!Bop.GE ) &&& (conde [(geo x y) &&& (v === (integer 1)); (lto x y) &&& (v === (integer 0))]);

      (op === !!Lang.Bop.OR ) &&& (conde [
        (nullo x)     &&& (nullo y)     &&& (v === (integer 0));
        (not_nullo x) &&& (nullo y)     &&& (v === (integer 1));
        (nullo x)     &&& (not_nullo y) &&& (v === (integer 1));
        (not_nullo x) &&& (not_nullo y) &&& (v === (integer 1));
      ]);

      (op === !!Lang.Bop.AND) &&& (conde [
        (nullo x)     &&& (nullo y)     &&& (v === (integer 0));
        (not_nullo x) &&& (nullo y)     &&& (v === (integer 0));
        (nullo x)     &&& (not_nullo y) &&& (v === (integer 0));
        (not_nullo x) &&& (not_nullo y) &&& (v === (integer 1));
      ]);
    ])
])

module Basic =
  struct
    let asserto label ctx ctx' t t' =
      fresh (e rs v h)
        (t  === assertion e)
        (t' === skip ())
        (Context.regso ctx rs)
        (expr_evalo rs e v)
        (conde [
          fresh (h)
            (Lang.Value.nullo v)
            (ctx' === Context.hole h)
            (label === Label.assert_fail ());

          (Lang.Value.not_nullo v) &&& (ctx === ctx') &&& (label === Label.empty ());
        ])

    let asgno label ctx ctx' t t' =
      fresh (r e v rs thrdId)
        (t  === asgn r e)
        (t' === skip ())
        (ctx === ctx')
        (label === Label.regwrite thrdId r v)
        (Context.regso ctx rs)
        (expr_evalo rs e v)
        (Context.thrdIdo ctx thrdId)

    let ifo label ctx ctx' t t' =
      fresh (e v rs t1 t2)
        (t === if' e t1 t2)
        (ctx === ctx')
        (label === Label.empty ())
        (Context.regso ctx rs)
        (expr_evalo rs e v)
        (conde [
          (Lang.Value.not_nullo v) &&& (t' === t1);
          (Lang.Value.nullo v)     &&& (t' === t2);
        ])

    let whileo label ctx ctx' t t' =
      fresh (e body u)
        (t  === while' e body)
        (t' === if' e (seq body t) (skip ()))
        (ctx === ctx')
        (label === Label.empty ())

    let repeato label ctx ctx' t t' =
      fresh (e body)
        (t  === repeat body e)
        (t' === seq body (while' (unop !!Uop.NOT e) body))
        (ctx === ctx')
        (label === Label.empty ())

    let seqo label ctx ctx' t t' =
      (t === seq (skip ()) t') &&&
      (ctx === ctx') &&&
      (label === Label.empty ())

    let all = [seqo; asgno; ifo; whileo; repeato; asserto;]

  end

module ThreadSpawning =
  struct
    let spawno label ctx ctx' t t' =
      fresh (t1 t2 thrdId)
       (t  === spw t1 t2)
       (t' === par t1 t2)
       (ctx === ctx')
       (label === Label.spawn thrdId)
       (Context.thrdIdo ctx thrdId)

    let joino label ctx ctx' t t' =
      fresh (t1 t2 thrdId)
        (t  === par t1 t2)
        (t' === skip ())
        (ctx === ctx')
        (label === Label.join thrdId)
        (Context.thrdIdo ctx thrdId)

    let returno label ctx ctx' t t' =
      fresh (t1 t2 thrdId rs)
        (t  === return rs)
        (t' === skip ())
        (ctx === ctx')
        (label === Label.return thrdId rs)
        (Context.thrdIdo ctx thrdId)

    let all = [spawno; joino; returno]
  end

module Atomic =
  struct
    let loado label ctx ctx' t t' =
      fresh (mo l r thrdId)
        (t  === load mo l r)
        (t' === skip ())
        (ctx === ctx')
        (label === Label.load thrdId mo l r)
        (Context.thrdIdo ctx thrdId)

    let storeo label ctx ctx' t t' =
      fresh (mo l e v rs thrdId)
        (t  === store mo l e)
        (t' === skip ())
        (ctx === ctx')
        (label === Label.store thrdId mo l v)
        (Context.regso ctx rs)
        (expr_evalo rs e v)
        (Context.thrdIdo ctx thrdId)

    let dataraceo label ctx ctx' t t' =
      fresh (mo l r e thrdId h)
        ((t === load mo l r) ||| (t === store mo l e))
        (t' === skip ())
        (ctx' === Context.hole h)
        (label === Label.datarace thrdId mo l)
        (Context.thrdIdo ctx thrdId)

    let all = [loado; storeo; dataraceo]
  end
