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

    let show = GT.show(logic) (fun s -> s)

    let pprint ff r = Format.fprintf ff "%s" @@ show r
  end

module Loc =
  struct
    type tt = string

    type tl = inner MiniKanren.logic
      and inner = string

    type ti = (tt, tl) MiniKanren.injected

    let loc l = !!l

    let reify = reify

    let show = GT.show(logic) (fun s -> s)

    let pprint ff l = Format.fprintf ff "%s" @@ show l
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

    let show n =
      pprint_nat Format.str_formatter n;
      Format.flush_str_formatter ()

    let pprint = pprint_nat

    let nullo v = (v === Nat.zero)

    let not_nullo v = fresh (x) (v === Nat.succ x)

    let addo = Nat.addo
    let mulo = Nat.mulo

    let eqo = (===)
    let nqo = (=/=)
    let lto = Nat.(<)
    let leo = Nat.(<=)
    let gto = Nat.(>)
    let geo = Nat.(>=)
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

    let pprint ff mo = Format.fprintf ff "%s" @@ show mo
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

    let pprint ff op = Format.fprintf ff "%s" @@ show op
  end

module ThreadID =
  struct
    module T =
      struct
        @type 'a t = N | L of 'a | R of 'a with gmap, show

        let fmap fa x = GT.gmap(t) fa x
        let show fa x = GT.show(t) fa x
      end

    include Fmap(T)

    type tt = tt T.t
    type tl = inner MiniKanren.logic
      and inner = tl T.t

    type ti = (tt, tl) MiniKanren.injected

    let reify' = reify
    let rec reify t = reify' (reify) t

    let pathn ()  = inj @@ distrib @@ T.N
    let pathl p   = inj @@ distrib @@ T.L p
    let pathr p   = inj @@ distrib @@ T.R p

    let rec show x = GT.show(logic) (T.show show) x

    let pprint ff thrdId = Format.fprintf ff "%s" @@ show thrdId
  end

module Expr =
  struct
    module T =
      struct
        @type ('var, 'value, 'op, 't) t =
          | Var     of 'var
          | Const   of 'value
          | Binop   of 'op * 't * 't
        with gmap, show

        let fmap fa fb fc fd x = GT.gmap(t) fa fb fc fd x
      end

    type tt = (Register.tt, Value.tt, Op.tt, tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (Register.tl, Value.tl, Op.tl, tl) T.t

    type ti = (tt, tl) Semantics.Term.ti

    module FT = Fmap4(T)

    let var x         = inj @@ FT.distrib @@ T.Var x
    let const v       = inj @@ FT.distrib @@ T.Const v
    let binop op l r  = inj @@ FT.distrib @@ T.Binop (op, l, r)

    let rec reify h = FT.reify (Register.reify) (Value.reify) (Op.reify) reify h

    let rec show t =
      GT.show(logic) (GT.show(T.t) Register.show Value.show Op.show show) t

    let rec pprint ff x = T.(
      let s ff = function
        | Var x            -> Format.fprintf ff "@[%a@]" Register.pprint x
        | Const n          -> Format.fprintf ff "@[%a@]" Value.pprint n
        | Binop (op, l, r) -> Format.fprintf ff "@[%a %a %a@]" pprint l Op.pprint op pprint r
      in
      pprint_logic s ff x
    )
  end

module Term =
  struct
    module T =
      struct
        @type ('reg, 'loc, 'mo, 'e, 't) t =
          | Skip
          | Stuck
          | Assert   of 'e
          | Asgn     of 'reg * 'e
          | If       of 'e * 't * 't
          | While    of 'e * 't
          | Load     of 'mo * 'loc * 'reg
          | Store    of 'mo * 'loc * 'e
          | Cas      of 'mo * 'mo * 'loc * 'e * 'e
          | Repeat   of 'mo * 'loc
          | Seq      of 't * 't
          | Spw      of 't * 't
          | Par      of 't * 't
        with gmap, show

        let fmap fa fb fc fd fe x = GT.gmap(t) fa fb fc fd fe x
      end

    type tt = (Register.tt, Loc.tt, MemOrder.tt, Expr.tt, tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (Register.tl, Loc.tl, MemOrder.tl, Expr.tl, tl) T.t

    type ti = (tt, tl) Semantics.Term.ti

    module FT = Fmap5(T)

    let assertion e         = inj @@ FT.distrib @@ T.Assert e
    let skip ()             = inj @@ FT.distrib @@ T.Skip
    let stuck ()            = inj @@ FT.distrib @@ T.Stuck
    let asgn r e            = inj @@ FT.distrib @@ T.Asgn (r, e)
    let if' e l r           = inj @@ FT.distrib @@ T.If (e, l, r)
    let while' e t          = inj @@ FT.distrib @@ T.While (e, t)
    let load mo l r         = inj @@ FT.distrib @@ T.Load (mo, l, r)
    let store mo l t        = inj @@ FT.distrib @@ T.Store (mo, l, t)
    let cas mo1 mo2 l e1 e2 = inj @@ FT.distrib @@ T.Cas (mo1, mo2, l, e1, e2)
    let repeat mo l         = inj @@ FT.distrib @@ T.Repeat (mo, l)
    let seq t1 t2           = inj @@ FT.distrib @@ T.Seq (t1, t2)
    let spw t1 t2           = inj @@ FT.distrib @@ T.Spw (t1, t2)
    let par t1 t2           = inj @@ FT.distrib @@ T.Par (t1, t2)

    let rec reify h = FT.reify Register.reify Loc.reify MemOrder.reify Expr.reify reify h

    let rec show t =
      GT.show(logic) (GT.show(T.t) (Register.show) (Loc.show) (MemOrder.show) (Expr.show) (show)) t

    let pprint = T.(
      let rec sl ff x = pprint_logic s ff x
      and s ff = function
        | Skip                    ->
          Format.fprintf ff "@[skip@]"
        | Stuck                   ->
          Format.fprintf ff "@[stuck@]"
        | Assert e                ->
          Format.fprintf ff "@[assert (%a)@;@]" Expr.pprint e
        | Asgn (r, e)             ->
          Format.fprintf ff "@[%a := %a@]" Register.pprint r Expr.pprint e
        | If (e, t1, t2)          ->
          Format.fprintf ff "@[<v>if %a@;then %a@;else %a@]" Expr.pprint e sl t1 sl t2
        | While (e, t)            ->
          Format.fprintf ff "@[while (%a) @;<1 4>%a@;@]" Expr.pprint e sl t
        | Load (m, l, r)          ->
          Format.fprintf ff "@[%a := %a_%a@]" Register.pprint r Loc.pprint l MemOrder.pprint m
        | Store (m, l, e)         ->
          Format.fprintf ff "@[%a_%a :=@;<1 4>%a@]" Loc.pprint l MemOrder.pprint m Expr.pprint e
        | Cas (m1, m2, l, e, d)   ->
          Format.fprintf ff "@[cas_%a_%a(%a, %a, %a)@]" MemOrder.pprint m1 MemOrder.pprint m2 Loc.pprint l Expr.pprint e Expr.pprint d
        | Repeat (m, l)           ->
          Format.fprintf ff "@[repeat %a_%a end@]" Loc.pprint l MemOrder.pprint m
        | Seq (t, t')             ->
          Format.fprintf ff "@[<v>%a;@;%a@]" sl t sl t'
        | Spw (t, t')             ->
          Format.fprintf ff "@[<v>spw {{{@;<1 4>%a@;|||@;<1 4>%a@;}}}@]" sl t sl t'
        | Par (t, t')             ->
          Format.fprintf ff "@[<v>par {{{@;<1 4>%a@;<1 4>|||@;<1 4>%a@;}}}@]" sl t sl t'
      in
      sl
    )

    let thrd_local_termo t = conde [
      fresh (e)
        (t === assertion e);

      fresh (r e)
        (t === asgn r e);

      fresh (e t1 t2)
        (t === if' e t1 t2);

      fresh (t1 t2)
        (t === seq t1 t2);
    ]

    let rec thrd_inter_termo t = conde [
      fresh (mo l r)
        (t === load mo l r);

      fresh (mo loc e)
        (t === store mo loc e);

      fresh (mo1 mo2 loc e d)
        (t === cas mo1 mo2 loc e d);

      fresh (loc mo)
        (t === repeat loc mo);

      fresh (t1 t2)
        (t === spw t1 t2);

      fresh (t1 t2)
        (t === par t1 t2);
    ]

    let rec irreducibleo t =
      (t === skip ()) ||| (t === stuck ())
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

module Label =
  struct
    module T =
      struct
        @type ('thrdId, 'mo, 'reg, 'loc, 'value) t =
          | Empty
          | Spawn     of 'thrdId
          | Join      of 'thrdId
          | RegRead   of 'thrdId * 'reg * 'value
          | RegWrite  of 'thrdId * 'reg * 'value
          | Load      of 'thrdId * 'mo * 'loc * 'reg
          | Store     of 'thrdId * 'mo * 'loc * 'value
          | Datarace  of 'thrdId * 'mo * 'loc
          | CAS       of 'thrdId * 'mo * 'mo * 'loc * 'value * 'value * 'value
        with gmap

        let fmap fa fb fc fd fe x = GT.gmap(t) (fa) (fb) (fc) (fd) (fe) x
      end

    type tt = (ThreadID.tt, MemOrder.tt, Register.tt, Loc.tt, Value.tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (ThreadID.tl, MemOrder.tl, Register.tl, Loc.tl, Value.tl) T.t

    type ti = (tt, tl) MiniKanren.injected

    module FT = Fmap5(T)

    let empty ()                      = inj @@ FT.distrib @@ T.Empty
    let spawn thrdId                  = inj @@ FT.distrib @@ T.Spawn thrdId
    let join  thrdId                  = inj @@ FT.distrib @@ T.Join  thrdId
    let regread  thrdId reg v         = inj @@ FT.distrib @@ T.RegRead  (thrdId, reg, v)
    let regwrite thrdId reg v         = inj @@ FT.distrib @@ T.RegWrite (thrdId, reg, v)
    let load  thrdId mo loc r         = inj @@ FT.distrib @@ T.Load  (thrdId, mo, loc, r)
    let store thrdId mo loc v         = inj @@ FT.distrib @@ T.Store (thrdId, mo, loc, v)
    let datarace thrdId mo loc        = inj @@ FT.distrib @@ T.Datarace (thrdId, mo, loc)
    let cas thrdId mo1 mo2 loc e d v  = inj @@ FT.distrib @@ T.CAS (thrdId, mo1, mo2, loc, e, d, v)

    let reify h = FT.reify ThreadID.reify MemOrder.reify Register.reify Loc.reify Value.reify h

    let pprint =
      let pp ff = T.(function
        | Empty ->
          Format.fprintf ff "@[<>@]"
        | Spawn thrdId ->
          Format.fprintf ff "@[<spawn %s>@]" (ThreadID.show thrdId)
        | Join thrdId ->
          Format.fprintf ff "@[<join %s>@]" (ThreadID.show thrdId)
        | RegRead (thrdId, reg, v) ->
          Format.fprintf ff "@[<regread %s %s %s>@]" (ThreadID.show thrdId) (Register.show reg) (Value.show v)
        | RegWrite (thrdId, reg, v) ->
          Format.fprintf ff "@[<regwrite %s %s %s>@]" (ThreadID.show thrdId) (Register.show reg) (Value.show v)
        | Load (thrdId, mo, loc, r) ->
          Format.fprintf ff "@[<load %s %s %s %s>@]" (ThreadID.show thrdId) (MemOrder.show mo) (Loc.show loc) (Register.show r)
        | Store (thrdId, mo, loc, v) ->
          Format.fprintf ff "@[<store %s %s %s %s>@]" (ThreadID.show thrdId) (MemOrder.show mo) (Loc.show loc) (Value.show v)
        | Datarace (thrdId, mo, loc) ->
          Format.fprintf ff "@[<datarace %s %s %s>@]" (ThreadID.show thrdId) (MemOrder.show mo) (Loc.show loc)
        | CAS (thrdId, mo1, mo2, loc, e, d, v) ->
          Format.fprintf ff "@[<CAS %s %s %s %s %s %s %s>@]"
            (ThreadID.show thrdId) (MemOrder.show mo1) (MemOrder.show mo2)
            (Loc.show loc) (Value.show e) (Value.show d) (Value.show v)
    )
    in
    pprint_logic pp

  end

let rec splito term ctx rdx = Term.(Context.(ThreadID.(conde [
  (* first we handle cases when term is simple and context is `empty`;
   * in this case the resulting split is equal to ({}, term),
   * when {} denotes `hole`, i.e. free variable
   *)
  fresh (h)
    (rdx === term)
    (ctx === hole h)
    (conde [
      (irreducibleo term);

      fresh (e)
        (term === assertion e);

      fresh (r e)
        (term === asgn r e);

      fresh (e t1 t2)
        (term === if' e t1 t2);

      fresh (mo l r)
        (term === load mo l r);

      fresh (mo loc e)
        (term === store mo loc e);

      fresh (mo1 mo2 loc e d)
        (term === cas mo1 mo2 loc e d);

      fresh (loc mo)
        (term === repeat loc mo);

      fresh (t1 t2)
        (term === seq t1 t2)
        (irreducibleo t1);

      fresh (t1 t2)
        (term === spw t1 t2);

      fresh (t1 t2)
        (term === par t1 t2)
        (irreducibleo t1)
        (irreducibleo t2);
    ]);

  (* second we handle complex terms *)
  fresh (h t t' ctx' thrdId thrdId')
    (ctx  === context t  h thrdId )
    (ctx' === context t' h thrdId')
    (conde [
      fresh (t1 t2)
        (term === seq t1 t2)
        (t    === seq t' t2)
        (thrdId === thrdId')
      ?~(irreducibleo t1)
        (splito t1 ctx' rdx);

      fresh (t1 t2)
        (term === par t1 t2)
      ?~((irreducibleo t1) &&& (irreducibleo t2))
        (conde [
            (t === par t' t2) &&&
            (thrdId === pathl thrdId') &&&
            (splito t1 ctx' rdx);

            (t === par t1 t') &&&
            (thrdId === pathr thrdId') &&&
            (splito t2 ctx' rdx);
        ])
    ]);
])))

let thrd_splito thrdId term ctx rdx =
  fresh (t h)
    (ctx === Context.context t h thrdId)
    (splito term ctx rdx)

let plugo ctx rdx term = Term.(Context.(conde [
  fresh (thrdId)
    (rdx  =/= stuck ())
    (term =/= stuck ())
    (ctx  === context term rdx thrdId);

  (rdx === stuck ()) &&& (term === stuck ());
]))
