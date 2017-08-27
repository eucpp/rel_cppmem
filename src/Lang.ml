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

    let reify = simple_reifier

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

    let reify = simple_reifier

    let inj = to_logic

    let show = GT.show(logic) (fun s -> s)
  end

module Value =
  struct
    type tt = tt lnat

    type tl = inner MiniKanren.logic
      and inner = tl lnat

    type ti = MiniKanrenStd.Nat.groundi

    let integer = inj_nat

    let zero () = Nat.zero
    let succ = Nat.succ

    let reify = Nat.reify

    let inj = Nat.to_logic

    (* let to_logic = Nat.to_logic *)

    (* let show = GT.show(Nat.logic) *)
    let show n =
      pprint_nat Format.str_formatter n;
      Format.flush_str_formatter ()

    let addo = Nat.addo
    let mulo = Nat.mulo

    let eqo = Nat.eqo
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

    let reify = simple_reifier

    let inj = to_logic

    let show = GT.show(logic) (to_string)
  end

module Op =
  struct
    type tt = ADD | MUL | EQ | NEQ | LT | LE | GT | GE

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

    let to_string = function
      | ADD   -> "+"
      | MUL   -> "-"
      | EQ    -> "="
      | NEQ   -> "!="
      | LT    -> "<"
      | LE    -> "<="
      | GT    -> ">"
      | GE    -> ">="

    let op s = !!(of_string s)

    let reify = simple_reifier

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
    let skip ()             = inj @@ FT.distrib @@ T.Skip
    let stuck ()            = inj @@ FT.distrib @@ T.Stuck

    let rec reify h =
      ManualReifiers.(FT.reify (string) (string) (Nat.reify) (simple_reifier) (simple_reifier) (reify) h)

    let inj' = inj

    let rec inj t =
      Value (T.fmap (Register.inj) (Loc.inj) (Value.inj) (MemOrder.inj) (Op.inj) (inj) t)

    (* let from_logic' = from_logic

    let rec from_logic = function
      | Value x    -> T.fmap (Nat.from_logic) (from_logic') (from_logic') (from_logic') (from_logic) x
      | Var (_, _) -> raise Not_a_value

    let rec to_logic x =
      let f x = Value x in
      Value (T.fmap (Nat.to_logic) (f) (f) (f) (to_logic) x) *)

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
        | Read (m, l)             -> Format.fprintf ff "@[%a_%a@]" loc l mo m
        | Write (m, l, t)         -> Format.fprintf ff "@[%a_%a :=@;<1 4>%a@]" loc l mo m sl t
        | Seq (t, t')             -> Format.fprintf ff "@[<v>%a;@;%a@]" sl t sl t'
        | Spw (t, t')             -> Format.fprintf ff "@[<v>spw {{{@;<1 4>%a@;|||@;<1 4>%a@;}}}@]" sl t sl t'
        | Par (t, t')             -> Format.fprintf ff "@[<v>par {{{@;<1 4>%a@;<1 4>|||@;<1 4>%a@;}}}@]" sl t sl t'
        | Skip                    -> Format.fprintf ff "@[skip@]"
        | Stuck                   -> Format.fprintf ff "@[stuck@]"
      in
      sl
    )

    let cond_expro e = conde [
      fresh (x mo)
        (e === read mo x);

      fresh (x mo n)
        (e  === binop !!Op.EQ (read mo x) (const Nat.zero));
    ]

    let rec stmto t = conde [
      fresh (x mo n)
        (t === write mo x (const n))
        (conde [
          (n === Nat.one);
          (n === Nat.zero);
        ]);

      fresh (e)
        (t === repeat e)
        (cond_expro e);

      fresh (e t1 t2)
        (t === if' e t1 t2)
        (cond_expro e)
        (seqo t1)
        (seqo t2);

      fresh (e body)
        (t === while' e body)
        (cond_expro e)
        (seqo body)
    ] and seqo t = conde [
      (stmto t);

      fresh (t1 t2)
        (t === seq t1 t2)
        (stmto t1)
        (seqo t2)
    ]

    let enumero = seqo

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

    include Fmap1(T)

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

    (* let inj' = inj

    let rec inj t  = inj' @@ distrib (T.fmap (Term.inj) (ThreadID.inj) t) *)

    let context : Term.ti -> Term.ti -> ThreadID.ti -> ti = fun term hole thrdId ->
      inj @@ distrib @@ T.({term; hole; thrdId})

    let hole h = context h h (ThreadID.pathn ())

    let thrdIdo ctx thrdId =
      fresh (term hole)
        (ctx === context term hole thrdId)
  end

let rec splito term result = Term.(Context.(ThreadID.(conde [
  fresh (op l r)
    (term === binop op l r)
    (conde [
      fresh (h)
        (splito l (Semantics.Split.undef ()))
        (splito r (Semantics.Split.undef ()))
        (result === Semantics.Split.split (hole h) term);

      fresh (ctx ctx' t t' h thrdId rdx)
        (splito l (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === binop op t' r)
        (result === Semantics.Split.split ctx rdx);

      fresh (ctx ctx' t t' h thrdId rdx)
        (splito l (Semantics.Split.undef ()))
        (splito r (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === binop op l t')
        (result === Semantics.Split.split ctx rdx);
    ]);

  fresh (t1 t2)
    (term === pair t1 t2)
    (conde [
      fresh (h)
        (splito t1 (Semantics.Split.undef ()))
        (splito t2 (Semantics.Split.undef ()))
        (result === Semantics.Split.undef ());

      fresh (rdx ctx ctx' t t' h thrdId)
        (splito t1 (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === pair t' t2)
        (result === Semantics.Split.split ctx rdx);

      fresh (rdx ctx ctx' t t' h thrdId)
        (splito t1 (Semantics.Split.undef ()))
        (splito t2 (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === pair t1 t')
        (result === Semantics.Split.split ctx rdx);
    ]);

  fresh (l r ctx')
    (term === asgn l r)
    (conde [
      fresh (h)
        (splito r (Semantics.Split.undef ()))
        (result === Semantics.Split.split (hole h) term);

      fresh (rdx ctx ctx' t t' h thrdId)
        (splito r (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === asgn l t')
        (result === Semantics.Split.split ctx rdx);
    ]);

  fresh (mo loc e ctx')
    (term === write mo loc e)
    (conde [
      fresh (h)
        (splito e (Semantics.Split.undef ()))
        (result === Semantics.Split.split (hole h) term);

      fresh (rdx ctx ctx' t t' h thrdId)
        (splito e (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === write mo loc t')
        (result === Semantics.Split.split ctx rdx);
    ]);

  fresh (cond btrue bfalse ctx')
    (term === if' cond btrue bfalse)
    (conde [
      fresh (h)
        (splito cond (Semantics.Split.undef ()))
        (result === Semantics.Split.split (hole h) term);

      fresh (rdx ctx ctx' t t' h thrdId)
        (splito cond (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === if' t' btrue bfalse)
        (result === Semantics.Split.split ctx rdx);
    ]);

  fresh (t1 t2 ctx')
    (term === seq t1 t2)
    (conde [
      fresh (h)
        (splito t1 (Semantics.Split.undef ()))
        (result === Semantics.Split.split (hole h) term);

      fresh (rdx ctx ctx' t t' h thrdId)
        (splito t1 (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h thrdId)
        (ctx' === context t' h thrdId)
        (t === seq t' t2)
        (result === Semantics.Split.split ctx rdx);
    ]);

  fresh (t1 t2 ctx' thrdId')
    (term === par t1 t2)
    (conde [
      fresh (h)
        (splito t1 (Semantics.Split.undef ()))
        (splito t2 (Semantics.Split.undef ()))
        (result === Semantics.Split.split (hole h) term);

      fresh (rdx ctx ctx' t t' h thrdId)
        (splito t1 (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h (pathl thrdId))
        (ctx' === context t' h thrdId)
        (t === par t' t2)
        (result === Semantics.Split.split ctx rdx);

      fresh (rdx ctx ctx' t t' h thrdId)
        (splito t2 (Semantics.Split.split ctx' rdx))
        (ctx  === context t  h (pathr thrdId))
        (ctx' === context t' h thrdId)
        (t === par t1 t')
        (result === Semantics.Split.split ctx rdx);
    ]);

  fresh (h)
    (result === Semantics.Split.split (hole h) term)
    (conde [
      fresh (x)
        (term === var x);

      fresh (loop)
        (term === repeat loop);

      fresh (mo l)
        (term === read mo l);

      fresh (mo1 mo2 l t1 t2)
        (term === cas mo1 mo2 l t1 t2);

      fresh (t1 t2)
        (term === spw t1 t2);
    ]);

  (result === Semantics.Split.undef ()) &&& (conde [
    fresh (n)
      (term === const n);

    (term === skip ());

    (term === stuck ());
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
    (rdx =/= stuck ())
    (ctx === context term rdx thrdId);

  (rdx === stuck ()) &&& (term === stuck ());
]))
