open MiniKanren
open MiniKanrenStd
open Utils

module Register =
  struct
    type tt = string

    type tl = inner MiniKanren.logic
      and inner = string

    type ti = (tt, tl) MiniKanren.injected

    let reg r = !!r

    let reify = reify

    let inj = to_logic

    let show = GT.show(logic) (fun s -> s)
  end

module Loc =
  struct
    type tt = string

    type tl = inner MiniKanren.logic
      and inner = string

    type ti = (tt, tl) MiniKanren.injected

    let loc l = !!l

    let reify = reify

    let inj = to_logic

    let show = GT.show(logic) (fun s -> s)
  end

module Value =
  struct
    type tt = tt nat

    type tl = inner MiniKanren.logic
      and inner = tl nat

    type ti = MiniKanrenStd.Nat.groundi

    let integer = nat

    let reify = Nat.reify

    let inj = Nat.inj

    (* let to_logic = Nat.inj *)

    (* let show = GT.show(Nat.logic) *)
    let show n =
      pprint_nat Format.str_formatter n;
      Format.flush_str_formatter ()

    let nullo v = (v === Nat.zero)

    let not_nullo v = fresh (x) (v === Nat.succ x)

    let addo = Nat.addo
    let mulo = Nat.mulo

    let eqo x y b = conde [
      (x === y) &&& (b === !!true);
      (x =/= y) &&& (b === !!false);
    ]

    let lto = Nat.lto
    let leo = Nat.leo
    let gto = Nat.gto
    let geo = Nat.geo
  end

module MemOrder =
  struct
    type tt = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    let of_string = function
      | "sc"      -> SC
      | "acq"     -> ACQ
      | "rel"     -> REL
      | "relAcq"  -> ACQ_REL
      | "con"     -> CON
      | "rlx"     -> RLX
      | "na"      -> NA

    let to_string = function
      | SC      -> "sc"
      | ACQ     -> "acq"
      | REL     -> "rel"
      | ACQ_REL -> "relAcq"
      | CON     -> "con"
      | RLX     -> "rlx"
      | NA      -> "na"

    let mo s = !!(of_string s)

    let reify = reify

    let inj = to_logic

    let show = GT.show(logic) (to_string)
  end

module Op =
  struct
    type tt = ADD | MUL | EQ | NEQ | LT | LE | GT | GE | OR | AND

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    let of_string = function
      | "+"   -> ADD
      | "*"   -> MUL
      | "="   -> EQ
      | "!="  -> NEQ
      | "<"   -> LT
      | "<="  -> LE
      | ">"   -> GT
      | ">="  -> GE
      | "||"  -> OR
      | "&&"  -> AND

    let to_string = function
      | ADD   -> "+"
      | MUL   -> "*"
      | EQ    -> "="
      | NEQ   -> "!="
      | LT    -> "<"
      | LE    -> "<="
      | GT    -> ">"
      | GE    -> ">="
      | OR    -> "||"
      | AND   -> "&&"

    let op s = !!(of_string s)

    let reify = reify

    let inj = to_logic

    let show = GT.show(logic) (to_string)
  end

module Term =
  struct
    module T =
      struct
        @type ('reg, 'loc, 'value, 'mo, 'op, 't) t =
          | Const    of 'value
          | Var      of 'reg
          | Binop    of 'op * 't * 't
          | Asgn     of 't * 't
          | Pair     of 't * 't
          | If       of 't * 't * 't
          | Repeat   of 't
          | While    of 't * 't
          | Read     of 'mo * 'loc
          | Write    of 'mo * 'loc * 't
          | Cas      of 'mo * 'mo * 'loc * 't * 't
          | Seq      of 't * 't
          | Spw      of 't * 't
          | Par      of 't * 't
          | Assert   of 't
          | Skip
          | Stuck
        with gmap, show

        let fmap fa fb fc fd fe ff x = GT.gmap(t) (fa) (fb) (fc) (fd) (fe) (ff) x
      end

    type tt = (Register.tt, Loc.tt, Value.tt, MemOrder.tt, Op.tt, tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (Register.tl, Loc.tl, Value.tl, MemOrder.tl, Op.tl, tl) T.t

    type ti = (tt, tl) Semantics.Term.ti

    module FT = Fmap6(T)

    let const n             = inj @@ FT.distrib @@ T.Const n
    let var x               = inj @@ FT.distrib @@ T.Var x
    let binop op l r        = inj @@ FT.distrib @@ T.Binop (op, l, r)
    let asgn l r            = inj @@ FT.distrib @@ T.Asgn (l, r)
    let pair l r            = inj @@ FT.distrib @@ T.Pair (l, r)
    let if' cond l r        = inj @@ FT.distrib @@ T.If (cond, l, r)
    let while' e t          = inj @@ FT.distrib @@ T.While (e, t)
    let repeat t            = inj @@ FT.distrib @@ T.Repeat t
    let read mo l           = inj @@ FT.distrib @@ T.Read (mo, l)
    let write mo l t        = inj @@ FT.distrib @@ T.Write (mo, l, t)
    let cas mo1 mo2 l t1 t2 = inj @@ FT.distrib @@ T.Cas (mo1, mo2, l, t1, t2)
    let seq t1 t2           = inj @@ FT.distrib @@ T.Seq (t1, t2)
    let spw t1 t2           = inj @@ FT.distrib @@ T.Spw (t1, t2)
    let par t1 t2           = inj @@ FT.distrib @@ T.Par (t1, t2)
    let assertion t         = inj @@ FT.distrib @@ T.Assert t
    let skip ()             = inj @@ FT.distrib @@ T.Skip
    let stuck ()            = inj @@ FT.distrib @@ T.Stuck

    let rec reify' h = FT.reify (reify) (reify) (Nat.reify) (reify) (reify) (reify') h

    let reify = reify'

    let rec inj t =
      to_logic (T.fmap (Register.inj) (Loc.inj) (Value.inj) (MemOrder.inj) (Op.inj) (inj) t)

    let rec show t =
      GT.show(logic) (GT.show(T.t) (Register.show) (Loc.show) (Value.show) (MemOrder.show) (Op.show) (show)) t

    let pprint = T.(
      let rec const   = pprint_nat    in
      let var         = pprint_string in
      let loc         = pprint_string in

      let kwd_op  = pprint_logic (fun ff op -> Format.fprintf ff "%s" (Op.to_string op)) in
      let mo      = pprint_logic (fun ff m  -> Format.fprintf ff "%s" (MemOrder.to_string m))  in

      let rec sl ff x = pprint_logic s ff x

      and s ff = function
        | Const n                 -> Format.fprintf ff "@[%a@]" const n
        | Var x                   -> Format.fprintf ff "@[%a@]" var x
        | Binop (op, a, b)        -> Format.fprintf ff "@[%a %a %a@]" sl a kwd_op op sl b
        | Asgn (x, y)             -> Format.fprintf ff "@[<hv>%a := %a@]" sl x sl y
        | Pair (x, y)             -> Format.fprintf ff "@[(%a, %a)@]" sl x sl y
        | If (cond, t, f)         -> Format.fprintf ff "@[<v>if %a@;then %a@;else %a@]" sl cond sl t sl f
        | Repeat t                -> Format.fprintf ff "@[repeat %a end@]" sl t
        | While (cond, t)         -> Format.fprintf ff "@[while (%a) @;<1 4>%a@;@]" sl cond sl t
        | Read (m, l)             -> Format.fprintf ff "@[%a_%a@]" loc l mo m
        | Write (m, l, t)         -> Format.fprintf ff "@[%a_%a :=@;<1 4>%a@]" loc l mo m sl t
        | Cas (m1, m2, l, e, d)   -> Format.fprintf ff "@[cas_%a_%a(%a, %a, %a)@]" mo m1 mo m2 loc l sl e sl d
        | Seq (t, t')             -> Format.fprintf ff "@[<v>%a;@;%a@]" sl t sl t'
        | Spw (t, t')             -> Format.fprintf ff "@[<v>spw {{{@;<1 4>%a@;|||@;<1 4>%a@;}}}@]" sl t sl t'
        | Par (t, t')             -> Format.fprintf ff "@[<v>par {{{@;<1 4>%a@;<1 4>|||@;<1 4>%a@;}}}@]" sl t sl t'
        | Assert t                -> Format.fprintf ff "@[assert %a@;]" sl t
        | Skip                    -> Format.fprintf ff "@[skip@]"
        | Stuck                   -> Format.fprintf ff "@[stuck@]"
      in
      sl
    )

    let bool_expro ?(loco= fun x -> success) e = conde [
      fresh (x mo)
        (loco x)
        (e === read mo x);

      fresh (x mo)
        (loco x)
        (e  === binop !!Op.EQ (read mo x) (const Nat.zero));

      fresh (r)
        (e === var r);

      (* test-and-set *)
      fresh (x mo1 mo2)
        (loco x)
        (e === cas mo1 mo2 x (const Nat.zero) (const Nat.one));

      (* test-and-set *)
      fresh (t x mo1 mo2)
        (loco x)
        (e === binop !!Op.EQ t (const Nat.zero))
        (t === cas mo1 mo2 x (const Nat.zero) (const Nat.one));
    ]

    let rec stmto ?(loco= fun x -> success) t = conde [
      fresh (x mo n)
        (loco x)
        (t === write mo x (const n))
        (conde [
          (n === Nat.one);
          (n === Nat.zero);
        ]);

      fresh (e)
        (t === repeat e)
        (bool_expro ~loco e);

      fresh (e t1 t2)
        (t === if' e t1 t2)
        (bool_expro ~loco e)
        (seq_stmto ~loco t1)
        (seq_stmto ~loco t2);

      fresh (e body)
        (t === while' e body)
        (bool_expro ~loco e)
        (seq_stmto ~loco body)
    ] and seq_stmto ?(loco= fun x -> success) t = conde [
      (stmto ~loco t);

      fresh (t1 t2)
        (t === seq t1 t2)
        (stmto ~loco t1)
        (seq_stmto ~loco t2)
    ]

  end

module ThreadID =
  struct
    module T =
      struct
        type 'a t = N | L of 'a | R of 'a

        let fmap ft = function
          | N   -> N
          | L p -> L (ft p)
          | R p -> R (ft p)
      end

    include Fmap(T)

    type tt = tt T.t
    type tl = inner MiniKanren.logic
      and inner = tl T.t

    type ti = (tt, tl) MiniKanren.injected

    let reify' = reify

    let rec reify t = reify' (reify) t

    let inj' = inj

    let rec inj p = inj' @@ distrib (T.fmap (inj) p)

    let pathn ()  = inj' @@ distrib @@ T.N
    let pathl p   = inj' @@ distrib @@ T.L p
    let pathr p   = inj' @@ distrib @@ T.R p
  end

module Context =
  struct
    module T =
      struct
        type ('t, 'thrdId) t = {
          term : 't;
          hole : 't;
          thrdId : 'thrdId;
        }

        let fmap fa fb {term; hole; thrdId} = {term = fa term; hole = fa hole; thrdId = fb thrdId}
      end

    type tt = (Term.tt, ThreadID.tt) T.t
    type tl = (Term.tl, ThreadID.tl) T.t MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    include Fmap2(T)

    let context : Term.ti -> Term.ti -> ThreadID.ti -> ti = fun term hole thrdId ->
      inj @@ distrib @@ T.({term; hole; thrdId})

    let hole h = context h h (ThreadID.pathn ())

    let thrdIdo ctx thrdId =
      fresh (term hole)
        (ctx === context term hole thrdId)
  end

let rec splito term result = Term.(Context.(ThreadID.(conde [
  (* first we handle cases when term is simple and context is `empty`;
   * in this case the resulting split is equal to ({}, term),
   * when {} denotes `hole`, i.e. free variable
   *)
  fresh (h)
    (result === Semantics.Split.split (hole h) term)
    (conde [
      fresh (x)
        (term === var x);

      fresh (op l r)
        (term === binop op l r)
        (splito l (Semantics.Split.undef ()))
        (splito r (Semantics.Split.undef ()));

      fresh (l r)
        (term === asgn l r)
        (splito r (Semantics.Split.undef ()));

      fresh (mo l)
        (term === read mo l);

      fresh (mo loc e)
        (term === write mo loc e)
        (splito e (Semantics.Split.undef ()));

      fresh (mo1 mo2 l t1 t2)
        (term === cas mo1 mo2 l t1 t2);

      fresh (e t1 t2)
        (term === if' e t1 t2)
        (splito e (Semantics.Split.undef ()));

      fresh (loop)
        (term === repeat loop);

      fresh (t1 t2)
        (term === seq t1 t2)
        (splito t1 (Semantics.Split.undef ()));

      fresh (t1 t2)
        (term === spw t1 t2);

      fresh (t1 t2)
        (term === par t1 t2)
        (splito t1 (Semantics.Split.undef ()))
        (splito t2 (Semantics.Split.undef ()));

      fresh (e)
        (term === assertion e)
        (splito e (Semantics.Split.undef ()));
    ]);

  (* here we handle irreducible terms *)
  (result === Semantics.Split.undef ()) &&& (conde [
    fresh (n)
      (term === const n);

    fresh (t1 t2)
      (term === pair t1 t2)
      (splito t1 (Semantics.Split.undef ()))
      (splito t2 (Semantics.Split.undef ()));

    (term === skip ());

    (term === stuck ());
  ]);

  (* finally we handle complex terms *)
  fresh (h t t' ctx ctx' rdx thrdId thrdId')
    (ctx    === context t  h thrdId )
    (ctx'   === context t' h thrdId')
    (result === Semantics.Split.split ctx rdx)
    (conde [
      (thrdId === thrdId') &&&
      (conde [
        fresh (op l r)
          (term === binop op l r)
          (conde [
              (t === binop op t' r) &&&
              (splito l (Semantics.Split.split ctx' rdx));

              (t === binop op l t') &&&
              (splito l (Semantics.Split.undef ())) &&&
              (splito r (Semantics.Split.split ctx' rdx));
          ]);

        fresh (t1 t2)
          (term === pair t1 t2)
          (conde [
              (t === pair t' t2) &&&
              (splito t1 (Semantics.Split.split ctx' rdx));

              (t === pair t1 t') &&&
              (splito t1 (Semantics.Split.undef ())) &&&
              (splito t2 (Semantics.Split.split ctx' rdx));
          ]);

        fresh (t1 t2)
          (term === asgn t1 t2)
          (t    === asgn t1 t')
          (splito t2 (Semantics.Split.split ctx' rdx));

        fresh (e t1 t2)
          (term === if' e  t1 t2)
          (t    === if' t' t1 t2)
          (splito e (Semantics.Split.split ctx' rdx));

        fresh (mo loc e)
          (term === write mo loc e )
          (t    === write mo loc t')
          (splito e (Semantics.Split.split ctx' rdx));

        fresh (e)
          (term === assertion e )
          (t    === assertion t')
          (splito e (Semantics.Split.split ctx' rdx));

        fresh (t1 t2)
          (term === seq t1 t2)
          (t    === seq t' t2)
          (splito t1 (Semantics.Split.split ctx' rdx));
      ]);

      fresh (t1 t2)
        (term === par t1 t2)
        (conde [
            (t === par t' t2) &&&
            (thrdId === pathl thrdId') &&&
            (splito t1 (Semantics.Split.split ctx' rdx));

            (t === par t1 t') &&&
            (thrdId === pathr thrdId') &&&
            (splito t2 (Semantics.Split.split ctx' rdx));
        ])
    ]);
])))

let rec promiseo term result = Term.(Context.(ThreadID.(conde [
  fresh (loc n h)
    (term === write !!MemOrder.RLX loc (const n))
    (result === Semantics.Split.split (hole h) term);

  fresh (t1 t2)
    (term === seq t1 t2)
    (conde [
      fresh (rdx ctx ctx' t t' h thrdId)
        (promiseo t1 (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === seq t' t2)
        (result === Semantics.Split.split ctx rdx);

      fresh (rdx ctx ctx' t t' h thrdId)
        (promiseo t2 (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === seq t1 t')
        (result === Semantics.Split.split ctx rdx);
    ]);

  fresh (t1 t2)
    (term === par t1 t2)
    (conde [
      fresh (rdx ctx ctx' t t' h thrdId)
        (promiseo t1 (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === par t' t2)
        (result === Semantics.Split.split ctx rdx);

      fresh (rdx ctx ctx' t t' h thrdId)
        (promiseo t2 (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === par t1 t')
        (result === Semantics.Split.split ctx rdx);
    ]);

  fresh (cond t1 t2)
    (term === if' cond t1 t2)
    (conde [
      fresh (rdx ctx ctx' t t' h thrdId)
        (promiseo t1 (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === if' cond t' t2)
        (result === Semantics.Split.split ctx rdx);

      fresh (rdx ctx ctx' t t' h thrdId)
        (promiseo t2 (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === if' cond t1 t')
        (result === Semantics.Split.split ctx rdx);
    ]);

  fresh (loop rdx ctx ctx' t t' h thrdId)
    (term === repeat loop)
    (promiseo loop (Semantics.Split.split ctx' rdx))
    (ctx  === context t  h thrdId)
    (ctx' === context t' h thrdId)
    (t === repeat t')
    (result === Semantics.Split.split ctx rdx);

])))

let plugo ctx rdx term = Term.(Context.(conde [
  fresh (thrdId)
    (rdx  =/= stuck ())
    (term =/= stuck ())
    (ctx === context term rdx thrdId);

  (rdx === stuck ()) &&& (term === stuck ());
]))
