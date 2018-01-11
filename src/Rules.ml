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

    let get_regso ctx tls =
      fresh (ctx')
        (ctx === context ctx' tls)

    let set_regso ctx ctx' tls' =
      fresh (tls ctx'')
        (ctx  === context ctx'' tls )
        (ctx' === context ctx'' tls')

    let thrdIdo ctx thrdId =
      fresh (ctx' tls)
        (ctx  === context ctx' tls)
        (Lang.Context.thrdIdo ctx' thrdId)
  end

type rule =
  Lang.Label.ti -> Context.ti -> Context.ti -> Lang.Term.ti -> Lang.Term.ti -> MiniKanren.goal

let rec expr_evalo rs e v = 

module Basic =
  struct
    let asserto label ctx ctx' t t' =
      fresh (e rs v h)
        (t  === assertion e)
        (t' === skip ())
        (Context.get_regso ctx rs)
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
        (Context.get_regso ctx rs)
        (expr_evalo rs e v)
        (Context.thrdIdo ctx thrdId)

    let ifo label ctx ctx' t t' =
      fresh (e v rs t1 t2)
        (t === if' e t1 t2)
        (ctx === ctx')
        (label === Label.empty ())
        (Context.get_regso ctx rs)
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
      fresh (mo l r v rs rs' thrdId)
        (t  === load mo l r)
        (t' === skip ())
        (label === Label.load thrdId mo l v)
        (Context.thrdIdo ctx thrdId)
        (Context.get_regso ctx rs)
        (Context.set_regso ctx ctx' rs')
        (RegisterStorage.writeo rs rs' r v)

    let storeo label ctx ctx' t t' =
      fresh (mo l e v rs thrdId)
        (t  === store mo l e)
        (t' === skip ())
        (ctx === ctx')
        (label === Label.store thrdId mo l v)
        (Context.get_regso ctx rs)
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
