open MiniKanren
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
        with gmap

        let fmap fint fstring fmo floc ft x = GT.gmap(t) (fint) (fstring) (fmo) (floc) (ft) x
      end

    type tt  = (Value.tt, Var.tt, MemOrder.tt, Loc.tt, tt) T.t
    type tl  = (Value.tl, Var.tl, MemOrder.tl, Loc.tl, tl) T.t MiniKanren.logic
    type ti  = (tt, tl) MiniKanren.injected

    include Fmap5(T)

    let const n             = inj @@ distrib @@ T.Const n
    let var x               = inj @@ distrib @@ T.Var x
    let binop op l r        = inj @@ distrib @@ T.Binop (op, l, r)
    let asgn l r            = inj @@ distrib @@ T.Asgn (l, r)
    let pair l r            = inj @@ distrib @@ T.Pair (l, r)
    let if' cond l r        = inj @@ distrib @@ T.If (cond, l, r)
    let repeat t            = inj @@ distrib @@ T.Repeat t
    let read mo l           = inj @@ distrib @@ T.Read (mo, l)
    let write mo l t        = inj @@ distrib @@ T.Write (mo, l, t)
    let cas mo1 mo2 l t1 t2 = inj @@ distrib @@ T.Cas (mo1, mo2, l, t1, t2)
    let seq t1 t2           = inj @@ distrib @@ T.Seq (t1, t2)
    let spw t1 t2           = inj @@ distrib @@ T.Spw (t1, t2)
    let par t1 t2           = inj @@ distrib @@ T.Par (t1, t2)
    let skip ()             = inj @@ distrib @@ T.Skip
    let stuck ()            = inj @@ distrib @@ T.Stuck

    let inj' = inj

    let rec inj t  = inj' @@ distrib (T.fmap (Nat.inj) (!!) (!!) (!!) (inj) t)

    let from_logic' = from_logic

    let rec from_logic = function
      | Value x    -> T.fmap (Nat.from_logic) (from_logic') (from_logic') (from_logic') (from_logic) x
      | Var (_, _) -> raise Not_a_value

    let rec to_logic x =
      let f x = Value x in
      Value (T.fmap (Nat.to_logic) (f) (f) (f) (to_logic) x)

    let rec reify' h = ManualReifiers.(reify (Nat.reify) (string_reifier) (simple_reifier) (string_reifier) (reify') h)

    let refine rr = rr#refine reify' ~inj:to_logic

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

  end

module Context =
  struct
    module T =
      struct
        @type ('expr, 'string, 'mo, 'loc, 't, 'c) t =
          | Hole
          | BinopL    of 'string * 'c * 't
          | BinopR    of 'string * 't * 'c
          | PairL     of 'c * 't
          | PairR     of 't * 'c
          | AsgnC     of 't * 'c
          | WriteC    of 'mo * 'loc * 'c
          | IfC       of 'c * 't * 't
          | SeqL      of 'c * 't
          | SeqR      of 't * 'c
          | ParL      of 'c * 't
          | ParR      of 't * 'c
        with gmap

        let fmap fint fstring fmo floc ft fc x = GT.gmap(t) (fint) (fstring) (fmo) (floc) (ft) (fc) x
      end

    type tt  = (Value.tt, Var.tt, MemOrder.tt, Loc.tt, Term.tt, tt) T.t
    type tl  = (Value.tl, Var.tl, MemOrder.tl, Loc.tl, Term.tl, tl) T.t MiniKanren.logic
    type ti  = (tt, tl) MiniKanren.injected

    include Fmap6(T)

    let binop_left op l r       = inj @@ distrib @@ T.BinopL (op, l, r)
    let binop_right op l r      = inj @@ distrib @@ T.BinopR (op, l, r)
    let pair_left l r           = inj @@ distrib @@ T.PairL (l, r)
    let pair_right l r          = inj @@ distrib @@ T.PairR (l, r)
    let asgn_ctx l r            = inj @@ distrib @@ T.AsgnC (l, r)
    let write_ctx mo l c        = inj @@ distrib @@ T.WriteC (mo, l, c)
    let if_ctx cond l r         = inj @@ distrib @@ T.IfC (cond, l, r)
    let seq_left t1 t2          = inj @@ distrib @@ T.SeqL (t1, t2)
    let seq_right t1 t2         = inj @@ distrib @@ T.SeqR (t1, t2)
    let par_left t1 t2          = inj @@ distrib @@ T.ParL (t1, t2)
    let par_right t1 t2         = inj @@ distrib @@ T.ParR (t1, t2)
    let hole ()                 = inj @@ distrib @@ T.Hole

    let inj' = inj

    let rec inj c = inj' @@ distrib (T.fmap (Nat.inj) (!!) (!!) (!!) (Term.inj) (inj) c)

    let rec splito term c rdx = Term.(conde [
      fresh (op l r c' t')
        (term === binop op l r)
        (conde [
          ((c === hole ()) &&& (reducibleo l !!false) &&& (reducibleo r !!false) &&& (rdx === term));
          ((c === binop_left op c' r) &&& (reducibleo l !!true)
            &&& (rdx === t') &&& (splito l c' t'));
          ((c === binop_right op l c') &&& (reducibleo l !!false) &&& (reducibleo r !!true)
            &&& (rdx === t') &&& (splito r c' t'));
        ]);

      fresh (t1 t2 c' t')
        (term === pair t1 t2)
        (conde [
          ((c === hole ())          &&& (reducibleo t1 !!false) &&& (reducibleo t2 !!false) &&& (rdx === term));
          ((c === pair_left c' t2)  &&& (reducibleo t1 !!true)
            &&& (rdx === t') &&& (splito t1 c' t'));
          ((c === pair_right t1 c') &&& (reducibleo t1 !!false) &&& (reducibleo t2 !!true)
            &&& (rdx === t') &&& (splito t2 c' t'));
        ]);

      fresh (l r c' t')
        (term === asgn l r)
        (conde [
          ((c === hole ())       &&& (reducibleo r !!false) &&& (rdx === term));
          ((c === asgn_ctx l c') &&& (reducibleo r !!true) &&& (rdx === t') &&& (splito r c' t'));
        ]);

      fresh (mo loc e c' t')
        (term === write mo loc e)
        (conde [
          ((c === hole ())             &&& (reducibleo e !!false) &&& (rdx === term ));
          ((c === write_ctx mo loc c') &&& (reducibleo e !!true ) &&& (rdx === t') &&& (splito e c' t'));
        ]);

      fresh (cond btrue bfalse c' t')
        (term === if' cond btrue bfalse)
        (conde [
          ((c === hole ())                &&& (reducibleo cond !!false) &&& (rdx === term ));
          ((c === if_ctx c' btrue bfalse) &&& (reducibleo cond !!true ) &&& (rdx === t') &&& (splito cond c' t'))
        ]);

      fresh (t1 t2 c' t')
        (term === seq t1 t2)
        (conde [
          (c === hole ())        &&& (reducibleo t1 !!false) &&& (rdx === term);
          (c === seq_left c' t2) &&& (reducibleo t1 !!true)  &&& (rdx === t'  ) &&& (splito t1 c' t');
        ]);

      fresh (t1 t2 c' t')
        (term === par t1 t2)
        (conde [
           (* (c === hole ())         &&& (reducibleo t1 !!false) &&& (reducibleo t2 !!false) &&& (rdx === term );
           (c === par_left  c' t2) &&& (reducibleo t1 !!true)  &&& (rdx === t') &&& (splito t1 c' t');
           (c === par_right t1 c') &&& (reducibleo t2 !!true)  &&& (rdx === t') &&& (splito t2 c' t'); *)
           (c === hole ())         &&& (rdx === term);
           (c === par_left  c' t2) &&& (rdx === t') &&& (splito t1 c' t');
           (c === par_right t1 c') &&& (rdx === t') &&& (splito t2 c' t');
        ]);

      ((c === hole ()) &&& (rdx === term) &&& conde [
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
    ])

    let rec plugo term c rdx = Term.(conde [
      (term === stuck ()) &&& (rdx === stuck ());
      (term =/= stuck ()) &&& (rdx =/= stuck ()) &&& (conde [
        fresh (op l r c' t')
          (term === binop op l r)
          (conde [
            ((c === hole ())              &&& (rdx === term));
            ((c === binop_left  op c' r)  &&& (rdx === t') &&& (plugo l c' t'));
            ((c === binop_right op l c')  &&& (rdx === t') &&& (plugo r c' t'));
          ]);

        fresh (t1 t2 c' t')
          (term === pair t1 t2)
          (conde [
            ((c === hole ())          &&& (rdx === term ));
            ((c === pair_left  c' t2) &&& (rdx === t') &&& (plugo t1 c' t'));
            ((c === pair_right t1 c') &&& (rdx === t') &&& (plugo t2 c' t'));
          ]);

        fresh (l r c' t')
          (term === asgn l r)
          (conde [
            ((c === hole ())       &&& (rdx === term ));
            ((c === asgn_ctx l c') &&& (rdx === t') &&& (plugo r c' t'));
          ]);

        fresh (mo loc e c' t')
          (term === write mo loc e)
          (conde [
            ((c === hole ())             &&& (rdx === term ));
            ((c === write_ctx mo loc c') &&& (rdx === t') &&& (plugo e c' t'));
          ]);

        fresh (cond btrue bfalse c' t')
          (term === if' cond btrue bfalse)
          (conde [
            ((c === hole ())                &&& (rdx === term));
            ((c === if_ctx c' btrue bfalse) &&& (rdx === t') &&& (plugo cond c' t'))
          ]);

        fresh (t1 t2 c')
          (term === seq t1 t2)
          (conde [
            ((c === hole ())                 &&& (rdx === term));
            ((c === seq_left  c' t2)         &&& (plugo t1 c' rdx));
            ((c === seq_right t1 c')         &&& (plugo t2 c' rdx));
          ]);
          (* ((c === seq_ctx c' t2) &&& conde [
            (rdx =/= skip ()) &&& (rdx =/= stuck ()) &&& (term === seq t1 t2) &&& (plugo t1 c' rdx);
            (rdx === skip ())  &&& (term === t2);
            (rdx === stuck ()) &&& (term === stuck ());
          ]); *)

        fresh (t1 t2 c' t')
          (term === par t1 t2)
          (conde [
             ((c === hole ())         &&& (rdx === term ));
             ((c === par_left  c' t2) &&& (rdx === t') &&& (plugo t1 c' t'));
             ((c === par_right t1 c') &&& (rdx === t') &&& (plugo t2 c' t'));
          ]);

        ((c === hole ()) &&& (rdx === term) &&& conde [
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
        ]);
      ])])

    let rec patho c path = Term.(Path.(
        fresh (op mo loc t1 t2 t3 t' c' path')
          (conde [
            (c === hole ())               &&& (path === pathn ());
            (c === binop_left op c' t1)   &&& (patho c' path);
            (c === binop_right op t1 c')  &&& (patho c' path);
            (c === pair_left c' t2)       &&& (patho c' path);
            (c === pair_right t1 c')      &&& (patho c' path);
            (c === asgn_ctx t1 c')        &&& (patho c' path);
            (c === write_ctx mo loc c')   &&& (patho c' path);
            (c === if_ctx c' t2 t3)       &&& (patho c' path);
            (c === seq_left  c' t1)       &&& (patho c' path);
            (c === seq_right t1 c')       &&& (patho c' path);
            (c === par_left c' t1)        &&& (path === pathl path') &&& (patho c' path');
            (c === par_right t1 c')       &&& (path === pathr path') &&& (patho c' path');
          ])
      ))

    let rec can_prmo t b = Term.(conde [
        (term === write !!MemOrder.RLX loc (const n)) &&& (b === !!true);

        fresh (t1 t2 b1 b2)
          (term === seq t1 t2)
          (can_prmo t1 b1)
          (can_prmo t2 b2)
          (Bool.oro b1 b2 b);

        fresh (t1 t2 b1 b2)
          (term === par t1 t2)
          (can_prmo t1 b1)
          (can_prmo t2 b2)
          (Bool.oro b1 b2 b);
      ])

    let rec pick_prmo term c loc n = Term.(conde [
        (term === write !!MemOrder.RLX loc (const n)) &&& (c === hole ());

        fresh (t1 t2 c')
          (term === seq t1 t2)
          (conde [
            (c === seq_left  c' t2) &&& (pick_prmo t1 c' loc n);
            (c === seq_right t1 c') &&& (pick_prmo t2 c' loc n);
          ]);

        fresh (t1 t2 c')
          (term === par t1 t2)
          (conde [
            (c === par_left  c' t2) &&& (pick_prmo t1 c' loc n);
            (c === par_right t1 c') &&& (pick_prmo t2 c' loc n);
          ]);
      ])

  end

let make_thread_step rules thrd_path =
  let reducibleo t b = Context.thrd_reducibleo t thrd_path b in
  let stepo (t, s) (t', s') =
    fresh (c c' rdx rdx')
      (Context.splito t c rdx)
      (Context.patho c thrd_path)
      (Context.reducibleo rdx !!true)
      (conde @@ List.map (fun (name, rule) -> rule c rdx s c' rdx' s') rules)
      (Context.plugo t' c' rdx')
  in
  make_step_relation ~reducibleo ~stepo

let make_reduction_relation rules =
  let reducibleo = Context.reducibleo in
  let stepo (t, s) (t', s') =
    fresh (c c' rdx rdx')
      (Context.splito t c rdx)
      (Context.reducibleo rdx !!true)
      (conde @@ List.map (fun (name, rule) -> rule c rdx s c' rdx' s') rules)
      (Context.plugo t' c' rdx')
  in
  make_step_relation ~reducibleo ~stepo

let make_certified_relation thrd_rules spw_join_rules =
  let reducibleo = Context.reducibleo in

  let certifyo t s thrd_path =
    let module CertStep = (val make_thread_step thrd_rules thrd_path) in
    let module Cert     = Semantics.Make(CertStep) in
    Cert.(
      fresh (t' s')
        (Memory.MemState.certifyo s' thrd_path)
        ((t, s) -->* (t', s'))
    )
  in

  let prm_stepo (t, s) (t', s') =
    fresh (c path loc n)
      (Context.pick_prmo t c loc n)
      (Context.patho c path)
      (Context.plugo t' c (Term.skip ()))
      (MemState.promiseo s s' path loc n)
  in

  let stepo' rules (t, c, s) (t', c', s') =
    fresh (rdx rdx')
      (Context.splito t c rdx)
      (Context.reducibleo rdx !!true)
      (conde @@ List.map (fun (name, rule) -> rule c rdx s c' rdx' s') rules)
      (Context.plugo t' c' rdx')
  in

  let stepo (t, s) (t', s') =
    let thrd_stepo = stepo' thrd_rules in
    let spw_join_stepo = stepo' spw_join_rules in
    fresh (c c' path)
      (conde [
        (prm_stepo (t, s) (t', s'));
        (thrd_stepo (t, c, s) (t', c', s')) &&& (Context.patho c path) &&& (certifyo t' s' path);
        (spw_join_stepo (t, c, s) (t', c', s'));
      ])
  in
  make_step_relation ~reducibleo ~stepo
