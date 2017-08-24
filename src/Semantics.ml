open MiniKanren

module Term =
  struct
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.injected
  end

module MaybeTerm =
  struct
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.Option.groundi

    let term t = Option.some t
    let undef () = Option.none ()
  end

module Context =
  struct
    type ('ct, 'cl) ti = ('ct, 'cl) MiniKanren.injected
  end

module Constraints =
  struct
    type ('cst, 'csl) ti = ('cst, 'csl) MiniKanren.injected
  end

module Split =
  struct
    module T =
      struct
        type ('t, 'c) t = {
          rdx : 't;
          ctx : 'c;
        }

        let fmap f g { rdx; ctx } = { rdx = f rdx; ctx = g ctx }
      end

    include T
    include Fmap2(T)

    type ('tt, 'ct) tt = ('tt, 'ct) T.t MiniKanren.Option.ground
    type ('tl, 'cl) tl = ('tl, 'cl) T.t MiniKanren.logic MiniKanren.Option.logic

    type ('tt, 'ct, 'tl, 'cl) ti = (('tt, 'ct) tt, ('tl, 'cl) tl) MiniKanren.injected

    let split ctx rdx = Option.some (inj @@ distrib @@ T.({ctx; rdx}))
    let undef () = Option.none ()

    let redexo s rdx =
      fresh (ctx)
        (s === split ctx rdx)

    let contexto s ctx =
      fresh (rdx)
        (s === split ctx rdx)
  end

type ('tt, 'ct, 'tl, 'cl) splitting =
  ('tt, 'tl) Term.ti -> ('tt, 'ct, 'tl, 'cl) Split.ti -> goal

type ('tt, 'ct, 'tl, 'cl) plugging =
  ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> goal

type ('tt, 'ct, 'cst, 'tl, 'cl, 'csl) rule =
  ('cst, 'csl) Constraints.ti -> ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> goal

(** Configuration - special case of Term for languages that distinguish a program and a state/environment *)
module Configuration =
  struct
    module T =
      struct
        type ('p, 's) t = {
          prog  : 'p;
          state : 's;
        }

        let fmap f g { prog; state } = { prog = f prog; state = g state }
      end

    include T
    include Fmap2(T)

    type ('pt, 'st) tt = ('pt, 'st) t
    type ('pl, 'sl) tl = ('pl, 'sl) t MiniKanren.logic

    type ('pt, 'st, 'pl, 'sl) ti = (('pt, 'st) tt, ('pl, 'sl) tl) MiniKanren.injected

    type ('tt, 'ct, 'cst, 'tl, 'cl, 'csl) rule' = ('tt, 'ct, 'cst, 'tl, 'cl, 'csl) rule

    type ('pt, 'ct, 'st, 'cst, 'pl, 'cl, 'sl, 'csl) rule =
      ('cst, 'csl) Constraints.ti -> ('ct, 'cl) Context.ti ->
      ('pt, 'pl) MiniKanren.injected -> ('st, 'sl) MiniKanren.injected -> ('pt, 'pl) MiniKanren.injected -> ('st, 'sl) MiniKanren.injected -> MiniKanren.goal

    let cfg prog state =
      inj @@ distrib @@ { prog; state }

    let inj inj_p inj_s t = Value (T.fmap inj_p inj_s t)

    let reify reify_p reify_s = reify reify_p reify_s

    let programo t prog =
      fresh (state)
        (t === cfg prog state)

    let stateo t state =
      fresh (prog)
        (t === cfg prog state)

    let lift_splitting splito t result =
      fresh (prog state result')
        (t === cfg prog state)
        (splito prog result')
        (conde [
          (result' === Split.undef ()) &&& (result === Split.undef ());
          fresh (ctx rdx)
            (result' === Split.split ctx rdx)
            (result === Split.split ctx (cfg rdx state));
        ])

    let lift_plugging plugo ctx rdx term =
      fresh (prog state prog')
        (rdx  === cfg prog  state)
        (term === cfg prog' state)
        (plugo ctx prog prog')

    let lift_rule rule ctrs ctx t t' =
    fresh (prog state prog' state')
      (t  === cfg prog  state )
      (t' === cfg prog' state')
      (rule ctrs ctx prog state prog' state')

  end

type ('tt, 'cst, 'tl, 'csl) step =
  ('cst, 'csl) Constraints.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) MaybeTerm.ti -> goal

type ('tt, 'tl) eval =
  ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

let make_reduction_relation splito plugo rules = fun ctrs term result ->
  fresh (ctx_rdx)
    (splito term ctx_rdx)
    (conde [
      (ctx_rdx === Split.undef ()) &&& (result === MaybeTerm.undef ());
      fresh (ctx rdx rdx' term')
        (ctx_rdx === Split.split ctx rdx)
        (result === MaybeTerm.term term')
        (conde @@ List.map (fun rule -> rule ctrs ctx rdx rdx') rules)
        (plugo ctx rdx' term');
    ])

let make_eval stepo t t' =
  let evalo_norec evalo term term'' =
    fresh (ctrs result)
      (stepo ctrs term result)
      (conde [
        (result === MaybeTerm.undef ()) &&& (term === term'');
        fresh (term')
          (result === MaybeTerm.term term')
          (delay @@ fun () -> evalo term' term'');
      ])
  in
  let tbl  = make_table () in
  let evalo = ref (fun term term' -> assert false) in
  let evalo_tabled = (fun term term' -> tabled2 tbl (evalo_norec !evalo) term term') in
  evalo := evalo_tabled;
  evalo_tabled t t' 



  (* evalo_tabled *)

(* module type StepRelation =
  sig
    type tt
    type tl
    type ti = (tt, tl) MiniKanren.injected

    type st
    type sl
    type si = (st, sl) MiniKanren.injected

    type helper = ((tt * st) option, (tl * sl) MiniKanren.logic option MiniKanren.logic) MiniKanren.injected

    val (-->) : ti * si -> helper -> MiniKanren.goal
  end *)

  (* module UnionRelation
    (S1 : StepRelation)
    (S2 : StepRelation with
      type tt = S1.tt  and
      type tl = S1.tl  and
      type st = S1.st  and
      type sl = S1.sl) =
  struct
    type tt = S1.tt
    type tl = S1.tl
    type ti = (tt, tl) MiniKanren.injected

    type st = S1.st
    type sl = S1.sl
    type si = (st, sl) MiniKanren.injected

    type helper = ((tt * st) option, (tl * sl) MiniKanren.logic option MiniKanren.logic) MiniKanren.injected

    let (-->) t t' =
      fresh (t1 t2 a b)
        (S1.(-->) t t1)
        (S2.(-->) t t2)
        (conde [
          (t1 === Option.none ()) &&& (t' === t2);
          (t1 === Option.some a)  &&& (t2 === Option.none ()) &&& (t' === t1);
          (t1 === Option.some a)  &&& (t2 === Option.some b)  &&& ((t' === t1) ||| (t' === t2));
        ])

  end *)
