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

    let rec prealloc_t vars atomics = T.(function
      | Read  (_, Value x)
      | Write (_, Value x, _) ->
        if List.mem x atomics then (vars, atomics) else (vars, x::atomics)
      | Var (Value x) ->
        if List.mem x vars then (vars, atomics) else (x::vars, atomics)
      | Repeat t1 ->
        prealloc_l vars atomics t1
      | Binop (_, t1, t2)
      | Asgn  (t1, t2)
      | Pair  (t1, t2)
      | Seq   (t1, t2)
      | Spw   (t1, t2)
      | Par   (t1, t2) ->
        let (vars', atomics') = prealloc_l vars atomics t1 in
          prealloc_l vars' atomics' t2
      | If (t1, t2, t3) ->
        let (vars' , atomics')  = prealloc_l vars atomics t1 in
        let (vars'', atomics'') = prealloc_l vars' atomics' t2 in
        prealloc_l vars'' atomics'' t3
      | _  -> (vars, atomics)
    )
    and prealloc_l vars atomics = function
      | Value t     -> prealloc_t vars atomics t
      | Var (i, []) -> (vars, atomics)

    let preallocate = prealloc_l [] []

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

    let rec reducibleo' term b = conde [
      fresh (n)
        (b === !!false)
        (term === const n);
      fresh (x)
        (b === !!true)
        (term === var x);
      fresh (op l r)
        (b === !!true)
        (term === binop op l r);
      fresh (l r)
        (b === !!true)
        (term === asgn l r);
      fresh (e t1 t2)
        (b === !!true)
        (term === if' e t1 t2);
      fresh (t')
        (b === !!true)
        (term === repeat t');
      fresh (mo l)
        (b === !!true)
        (term === read mo l);
      fresh (mo l t')
        (b === !!true)
        (term === write mo l t');
      fresh (mo1 mo2 l e1 e2)
        (b === !!true)
        (term === cas mo1 mo2 l e1 e2);
      fresh (t1 t2)
        (b === !!true)
        (term === seq t1 t2);
      fresh (t1 t2)
        (b === !!true)
        (term === spw t1 t2);
      fresh (t1 t2)
        (b === !!true)
        (term === par t1 t2);

      (conde [
         fresh (t1 t2 b1 b2)
           (term === pair t1 t2)
           (reducibleo' t1 b1)
           (reducibleo' t2 b2)
           (Bool.oro b1 b2 b)
      ]);

      ((b === !!false) &&& (term === skip ()));
      ((b === !!false) &&& (term === stuck ()));
    ]

    let rec reducibleo ?path t b = Path.(
      match path with
        | Some p ->
          fresh (p' l r) (conde [
            (p === pathn ()) &&& (reducibleo' t b);
            (p === pathl p') &&& (t === par l r) &&& (reducibleo ~path:p' l b);
            (p === pathr p') &&& (t === par l r) &&& (reducibleo ~path:p' r b);
          ])
        | None ->
          reducibleo' t b
    )

    let rec can_prmo t b = conde [
      fresh (mo loc e n)
        (t === write mo loc e)
        (conde [
          (mo === !!MemOrder.RLX) &&& (conde [
            (e === const n) &&& (b === !!true);
            fresh (x)
              (e === var x) &&& (b === !!false);
            fresh (x mo')
              (e === read mo' x) &&& (b === !!false);
          ]);
          (mo =/= !!MemOrder.RLX) &&& (b === !!false);
        ]);

      fresh (t1 t2 b1 b2)
        (t === seq t1 t2)
        (can_prmo t1 b1)
        (can_prmo t2 b2)
        (Bool.oro b1 b2 b);

      fresh (t1 t2 b1 b2)
        (t === par t1 t2)
        (can_prmo t1 b1)
        (can_prmo t2 b2)
        (Bool.oro b1 b2 b);

      fresh (cond t1 t2 b1 b2)
        (t === if' cond t1 t2)
        (can_prmo t1 b1)
        (can_prmo t2 b2)
        (Bool.oro b1 b2 b);

      fresh (t')
        (t === repeat t')
        (can_prmo t' b);

      fresh (n)
        (t === const n)
        (b === !!false);
      fresh (x)
        (t === var x)
        (b === !!false);
      fresh (t1 t2)
        (t === pair t1 t2)
        (b === !!false);
      fresh (t1 t2)
        (t === asgn t1 t2)
        (b === !!false);
      fresh (mo l)
        (t === read mo l)
        (b === !!false);
      fresh (mo1 mo2 l t1 t2)
        (t === cas mo1 mo2 l t1 t2)
        (b === !!false);
      fresh (t1 t2)
        (t === spw t1 t2)
        (b === !!false);

      (t === skip ()) &&& (b === !!false);
    ]

  end

module Context =
  struct
    module T =
      struct
        type ('t, 'path) t = {
          ctx  : 't;
          hole : 't;
          path : 'path;
        }

        let fmap fa fb {ctx; hole; path} = {ctx = fa ctx; hole = fa hole; path = fb path}
      end

    type tt = (Term.tt, Memory.Path.tt) T.t
    type tl = (Term.tl, Memory.Path.tl) T.t MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    include Fmap2(T)

    let inj' = inj

    let rec inj t  = inj' @@ distrib (T.fmap (Term.inj) (Path.inj) t)

    let make_context : Term.ti -> Term.ti -> Path.ti -> ti = fun ctx hole path ->
      inj' @@ distrib @@ T.({ctx; hole; path})

    (* let hole () =  *)

    let rec splito' term rdx ctx hole path = Term.(Path.(conde [
      fresh (op l r ctx')
        (term === binop op l r)
        (conde [
          (reducibleo l !!false) &&& (reducibleo r !!false)
            &&& (ctx === hole) &&& (rdx === term) &&& (path === pathn ());

          (reducibleo l !!true)
            &&& (ctx === binop op ctx' r)
            &&& (splito' l rdx ctx' hole path);

          (reducibleo l !!false) &&& (reducibleo r !!true)
            &&& (ctx === binop op l ctx')
            &&& (splito' r rdx ctx' hole path);
        ]);

      fresh (t1 t2 ctx')
        (term === pair t1 t2)
        (conde [
          (reducibleo t1 !!false) &&& (reducibleo t2 !!false)
            &&& (ctx === hole) &&& (rdx === term) &&& (path === pathn ());

          (reducibleo t1 !!true)
            &&& (ctx === pair ctx' t2)
            &&& (splito' t1 rdx ctx' hole path);

          (reducibleo t1 !!false) &&& (reducibleo t2 !!true)
            &&& (ctx === pair t1 ctx')
            &&& (splito' t2 rdx ctx' hole path);
        ]);

      fresh (l r ctx')
        (term === asgn l r)
        (conde [
          (reducibleo r !!false)
            &&& (ctx === hole) &&& (rdx === term) &&& (path === pathn ());

          (reducibleo r !!true)
            &&& (ctx === asgn l ctx')
            &&& (splito' r rdx ctx' hole path);
        ]);

      fresh (mo loc e ctx')
        (term === write mo loc e)
        (conde [
          (reducibleo e !!false)
            &&& (ctx === hole) &&& (rdx === term) &&& (path === pathn ());

          (reducibleo e !!true)
            &&& (ctx === write mo loc ctx')
            &&& (splito' e rdx ctx' hole path);
        ]);

      fresh (cond btrue bfalse ctx')
        (term === if' cond btrue bfalse)
        (conde [
          (reducibleo cond !!false)
            &&& (ctx === hole) &&& (rdx === term) &&& (path === pathn ());

          (reducibleo cond !!true)
            &&& (ctx === if' ctx' btrue bfalse)
            &&& (splito' cond rdx ctx' hole path)
        ]);

      fresh (t1 t2 ctx')
        (term === seq t1 t2)
        (conde [
          (reducibleo t1 !!false)
            &&& (ctx === hole) &&& (rdx === term) &&& (path === pathn ());

          (reducibleo t1 !!true)
            &&& (ctx === seq ctx' t2)
            &&& (splito' t1 rdx ctx' hole path);
        ]);

      fresh (t1 t2 ctx' path')
        (term === par t1 t2)
        (conde [
          (reducibleo t1 !!false) &&& (reducibleo t2 !!false)
            &&& (ctx === hole) &&& (rdx === term) &&& (path === pathn ());

          (reducibleo t1 !!true)
            &&& (ctx === par ctx' t2) &&& (path === pathl path')
            &&& (splito' t1 rdx ctx' hole path');

          (reducibleo t2 !!true)
            &&& (ctx === par t1 ctx') &&& (path === pathr path')
            &&& (splito' t2 rdx ctx' hole path');
        ]);

      (ctx === hole) &&& (rdx === term) &&& (path === pathn ()) &&& (conde [
        fresh (n)
          (term === const n);

        fresh (x)
          (term === var x);

        fresh (t')
          (term === repeat t');

        fresh (mo l)
          (term === read mo l);

        fresh (mo1 mo2 l t1 t2)
          (term === cas mo1 mo2 l t1 t2);

        fresh (t1 t2)
          (term === spw t1 t2);

        (term === skip ());

        (term === stuck ());
      ]);
    ]))

    let rec pick_prmo' term rdx ctx hole path = Term.(Path.(conde [
      fresh (loc n)
        (term === write !!MemOrder.RLX loc (const n))
          &&& (ctx === hole) &&& (rdx === term) &&& (path === pathn ());

      fresh (t1 t2 ctx')
        (term === seq t1 t2)
        (conde [
          (ctx === seq ctx' t2) &&& (pick_prmo' t1 rdx ctx' hole path);
          (ctx === seq t1 ctx') &&& (pick_prmo' t2 rdx ctx' hole path);
        ]);

      fresh (t1 t2 ctx' path')
        (term === par t1 t2)
        (conde [
          (ctx === par ctx' t2) &&& (pick_prmo' t1 rdx ctx' hole path') &&& (path === pathl path');
          (ctx === par t1 ctx') &&& (pick_prmo' t2 rdx ctx' hole path') &&& (path === pathr path');
        ]);

      fresh (t1 t2 cond ctx')
        (term === if' cond t1 t2)
        (conde [
          (ctx === if' cond ctx' t2) &&& (pick_prmo' t1 rdx ctx' hole path);
          (ctx === if' cond t1 ctx') &&& (pick_prmo' t2 rdx ctx' hole path);
        ]);

      fresh (t' ctx')
        (term === repeat t')
        (ctx === repeat ctx')
        (pick_prmo' t' rdx ctx' hole path);
    ]))

    let dumb_splito term context rdx =
      fresh (ctx hole path)
        (context === make_context ctx hole path)
        (term === rdx)
        (term === ctx)
        (ctx  === hole)
        (path === Path.pathn ())

    let splito term context rdx =
      fresh (ctx hole path)
        (context === make_context ctx hole path)
        (splito' term rdx ctx hole path)

    let pick_prmo term context rdx =
      fresh (ctx hole path)
        (context === make_context ctx hole path)
        (pick_prmo' term rdx ctx hole path)

    let plugo term context rdx = Term.(conde [
      fresh (path)
        (rdx  =/= stuck ())
        (context === make_context term rdx path);

      (rdx === stuck ()) &&& (term === stuck ());
    ])

    let patho context path =
      fresh (ctx hole)
        (context === make_context ctx hole path)
  end
