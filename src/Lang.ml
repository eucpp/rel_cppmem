open MiniKanren
open MiniKanrenStd
open Memory
open Utils

module Term =
  struct
    module T =
      struct
        @type ('int, 'string, 'mo, 'loc, 't) t =
          | Const    of 'int
          | Var      of 'string
          | Binop    of 'string * 't * 't
          | Asgn     of 't * 't
          | Pair     of 't * 't
          | If       of 't * 't * 't
          | Repeat   of 't
          | Read     of 'mo * 'loc
          | Write    of 'mo * 'loc * 't
          | Cas      of 'mo * 'mo * 'loc * 't * 't
          | Seq      of 't * 't
          | Spw      of 't * 't
          | Par      of 't * 't
          | Skip
          | Stuck
        with gmap, show

        let fmap fint fstring fmo floc ft x = GT.gmap(t) (fint) (fstring) (fmo) (floc) (ft) x
      end

    type tt  = (Value.tt, Var.tt, MemOrder.tt, Loc.tt, tt) T.t
    type tl  = (Value.tl, Var.tl, MemOrder.tl, Loc.tl, tl) T.t MiniKanren.logic
    type ti  = (tt, tl) MiniKanren.injected

    module FT = Fmap5(T)

    let const n             = inj @@ FT.distrib @@ T.Const n
    let var x               = inj @@ FT.distrib @@ T.Var x
    let binop op l r        = inj @@ FT.distrib @@ T.Binop (op, l, r)
    let asgn l r            = inj @@ FT.distrib @@ T.Asgn (l, r)
    let pair l r            = inj @@ FT.distrib @@ T.Pair (l, r)
    let if' cond l r        = inj @@ FT.distrib @@ T.If (cond, l, r)
    let repeat t            = inj @@ FT.distrib @@ T.Repeat t
    let read mo l           = inj @@ FT.distrib @@ T.Read (mo, l)
    let write mo l t        = inj @@ FT.distrib @@ T.Write (mo, l, t)
    let cas mo1 mo2 l t1 t2 = inj @@ FT.distrib @@ T.Cas (mo1, mo2, l, t1, t2)
    let seq t1 t2           = inj @@ FT.distrib @@ T.Seq (t1, t2)
    let spw t1 t2           = inj @@ FT.distrib @@ T.Spw (t1, t2)
    let par t1 t2           = inj @@ FT.distrib @@ T.Par (t1, t2)
    let skip ()             = inj @@ FT.distrib @@ T.Skip
    let stuck ()            = inj @@ FT.distrib @@ T.Stuck

    let inj' = inj

    let rec inj t  = inj' @@ FT.distrib (T.fmap (Nat.inj) (!!) (!!) (!!) (inj) t)

    let from_logic' = from_logic

    let rec from_logic = function
      | Value x    -> T.fmap (Nat.from_logic) (from_logic') (from_logic') (from_logic') (from_logic) x
      | Var (_, _) -> raise Not_a_value

    let rec to_logic x =
      let f x = Value x in
      Value (T.fmap (Nat.to_logic) (f) (f) (f) (to_logic) x)

    let rec reify h = ManualReifiers.(FT.reify (Nat.reify) (string) (simple_reifier) (string) (reify) h)

    let refine rr = rr#refine reify ~inj:to_logic

    let rec show t = GT.show(logic) (GT.show(T.t) (Value.show) (Var.show) (MemOrder.show) (Loc.show) (show)) t

    let pprint term = T.(
      let rec const   = pprint_nat    in
      let kwd         = pprint_string in
      let var         = pprint_string in
      let loc         = pprint_string in

      let mo ff x         = pprint_logic (fun ff m -> Format.fprintf ff "%s" (MemOrder.to_string m)) ff x in
      let rec sl ff x     = pprint_logic s ff x

      and s ff = function
        | Const n                 -> Format.fprintf ff "@[%a@]" const n
        | Var x                   -> Format.fprintf ff "@[%a@]" var x
        | Binop (op, a, b)        -> Format.fprintf ff "@[%a %a %a@]" sl a kwd op sl b
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
      sl Format.str_formatter term;
      Format.flush_str_formatter ()
    )

  end

module Context =
  struct
    module T =
      struct
        type ('t, 'path) t = {
          term : 't;
          hole : 't;
          path : 'path;
        }

        let fmap fa fb {term; hole; path} = {term = fa term; hole = fa hole; path = fb path}
      end

    type tt = (Term.tt, Memory.Path.tt) T.t
    type tl = (Term.tl, Memory.Path.tl) T.t MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    include Fmap2(T)

    (* let inj' = inj

    let rec inj t  = inj' @@ distrib (T.fmap (Term.inj) (Path.inj) t) *)

    let context : Term.ti -> Term.ti -> Path.ti -> ti = fun term hole path ->
      inj @@ distrib @@ T.({term; hole; path})

    let hole h = context h h (Path.pathn ())

    let patho ctx path =
      fresh (term hole)
        (ctx === context term hole path)
  end

module Decay =
  struct
    module T =
      struct
        type ('t, 'c) t = {
          rdx : 't;
          ctx : 'c;
        }

        let fmap fa fb {rdx; ctx} = {rdx = fa rdx; ctx = fb ctx}
      end

    type tt = (Term.tt, Context.tt) T.t option
    type tl = (Term.tl, Context.tl) T.t MiniKanren.logic option MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    include Fmap2(T)

    let none () = MiniKanrenStd.Option.none ()
    let decay ctx rdx = MiniKanrenStd.Option.some (inj @@ distrib @@ T.({ctx; rdx}))

    let redexo dec rdx =
      fresh (ctx)
        (dec === decay ctx rdx)

    let contexto dec ctx =
      fresh (rdx)
        (dec === decay ctx rdx)
  end

let rec splito term dec = Term.(Context.(Path.(Decay.(conde [
  fresh (op l r)
    (term === binop op l r)
    (conde [
      fresh (h)
        (splito l (none ()))
        (splito r (none ()))
        (dec === decay (hole h) term);

      fresh (ctx ctx' t t' h path rdx)
        (splito l (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === binop op t' r)
        (dec === decay ctx rdx);

      fresh (ctx ctx' t t' h path rdx)
        (splito l (none ()))
        (splito r (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === binop op l t')
        (dec === decay ctx rdx);
    ]);

  fresh (t1 t2)
    (term === pair t1 t2)
    (conde [
      fresh (h)
        (splito t1 (none ()))
        (splito t2 (none ()))
        (dec === none ());

      fresh (rdx ctx ctx' t t' h path)
        (splito t1 (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === pair t' t2)
        (dec === decay ctx rdx);

      fresh (rdx ctx ctx' t t' h path)
        (splito t1 (none ()))
        (splito t2 (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === pair t1 t')
        (dec === decay ctx rdx);
    ]);

  fresh (l r ctx')
    (term === asgn l r)
    (conde [
      fresh (h)
        (splito r (none ()))
        (dec === decay (hole h) term);

      fresh (rdx ctx ctx' t t' h path)
        (splito r (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === asgn l t')
        (dec === decay ctx rdx);
    ]);

  fresh (mo loc e ctx')
    (term === write mo loc e)
    (conde [
      fresh (h)
        (splito e (none ()))
        (dec === decay (hole h) term);

      fresh (rdx ctx ctx' t t' h path)
        (splito e (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === write mo loc t')
        (dec === decay ctx rdx);
    ]);

  fresh (cond btrue bfalse ctx')
    (term === if' cond btrue bfalse)
    (conde [
      fresh (h)
        (splito cond (none ()))
        (dec === decay (hole h) term);

      fresh (rdx ctx ctx' t t' h path)
        (splito cond (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === if' t' btrue bfalse)
        (dec === decay ctx rdx);
    ]);

  fresh (t1 t2 ctx')
    (term === seq t1 t2)
    (conde [
      fresh (h)
        (splito t1 (none ()))
        (dec === decay (hole h) term);

      fresh (rdx ctx ctx' t t' h path)
        (splito t1 (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === seq t' t2)
        (dec === decay ctx rdx);
    ]);

  fresh (t1 t2 ctx' path')
    (term === par t1 t2)
    (conde [
      fresh (h)
        (splito t1 (none ()))
        (splito t2 (none ()))
        (dec === decay (hole h) term);

      fresh (rdx ctx ctx' t t' h path)
        (splito t1 (decay ctx' rdx))
        (ctx  === context t  h (pathl path))
        (ctx' === context t' h path)
        (t === par t' t2)
        (dec === decay ctx rdx);

      fresh (rdx ctx ctx' t t' h path)
        (splito t2 (decay ctx' rdx))
        (ctx  === context t  h (pathr path))
        (ctx' === context t' h path)
        (t === par t1 t')
        (dec === decay ctx rdx);
    ]);

  fresh (h)
    (dec === decay (hole h) term)
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

  (dec === none ()) &&& (conde [
    fresh (n)
      (term === const n);

    (term === skip ());

    (term === stuck ());
  ]);

]))))

let thrd_splito path term dec = Context.(Decay.(
  (conde [
    (dec === none ());

    fresh (ctx rdx t hole)
      (dec === decay ctx rdx)
      (ctx === context t hole path);
  ])
  &&&
  (splito term dec)
))

let rec promiseo term dec = Term.(Context.(Decay.(Path.(conde [
  fresh (loc n h)
    (term === write !!MemOrder.RLX loc (const n))
    (dec === decay (hole h) term);

  fresh (t1 t2)
    (term === seq t1 t2)
    (conde [
      fresh (rdx ctx ctx' t t' h path)
        (promiseo t1 (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === seq t' t2)
        (dec === decay ctx rdx);

      fresh (rdx ctx ctx' t t' h path)
        (promiseo t2 (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === seq t1 t')
        (dec === decay ctx rdx);
    ]);

  fresh (t1 t2)
    (term === par t1 t2)
    (conde [
      fresh (rdx ctx ctx' t t' h path)
        (promiseo t1 (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === par t' t2)
        (dec === decay ctx rdx);

      fresh (rdx ctx ctx' t t' h path)
        (promiseo t2 (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === par t1 t')
        (dec === decay ctx rdx);
    ]);

  fresh (cond t1 t2)
    (term === if' cond t1 t2)
    (conde [
      fresh (rdx ctx ctx' t t' h path)
        (promiseo t1 (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === if' cond t' t2)
        (dec === decay ctx rdx);

      fresh (rdx ctx ctx' t t' h path)
        (promiseo t2 (decay ctx' rdx))
        (ctx  === context t  h path)
        (ctx' === context t' h path)
        (t === if' cond t1 t')
        (dec === decay ctx rdx);
    ]);

  fresh (loop rdx ctx ctx' t t' h path)
    (term === repeat loop)
    (promiseo loop (decay ctx' rdx))
    (ctx  === context t  h path)
    (ctx' === context t' h path)
    (t === repeat t')
    (dec === decay ctx rdx);

]))))

let plugo ctx rdx term = Term.(Context.(Decay.(conde [
  fresh (path)
    (rdx =/= stuck ())
    (ctx === context term rdx path);

  (rdx === stuck ()) &&& (term === stuck ());
])))
