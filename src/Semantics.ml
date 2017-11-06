open MiniKanren
open MiniKanren.Std

module Term =
  struct
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.injected
  end

module MaybeTerm =
  struct
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.Std.Option.groundi

    let term t = Option.some t
    let undef () = Option.none ()
  end

module Context =
  struct
    type ('ct, 'cl) ti = ('ct, 'cl) MiniKanren.injected
  end

module Label =
  struct
    type ('lt, 'll) ti = ('lt, 'll) MiniKanren.injected
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

    type ('tt, 'ct) tt = ('tt, 'ct) T.t MiniKanren.Std.Option.ground
    type ('tl, 'cl) tl = ('tl, 'cl) T.t MiniKanren.logic MiniKanren.Std.Option.logic

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
  ('tt, 'tl) Term.ti -> ('tt, 'ct, 'tl, 'cl) Split.ti -> MiniKanren.goal

type ('tt, 'ct, 'tl, 'cl) plugging =
  ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

type ('tt, 'ct, 'tl, 'cl) rule =
  ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

module type State =
  sig
    include Utils.Logic

    val transitiono : ('lt, 'll) Label.ti -> ti -> ti -> MiniKanren.goal
  end

module TransitionLabeledSystem (P : Utils.Logic) (S : State) =
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

    type tt = (P.tt, S.tt) t
    type tl =  inner MiniKanren.logic
      and inner = (P.tl, S.tl) t

    type ti = (tt, tl) MiniKanren.injected

    type ('tt, 'ct, 'tl, 'cl) rule' = ('tt, 'ct, 'tl, 'cl) rule

    type ('ct, 'lt, 'cl, 'll) rule =
      ('lt, 'll) Label.ti -> ('ct, 'cl) Context.ti -> P.ti -> P.ti -> MiniKanren.goal

    let init prog state =
      inj @@ distrib @@ { prog; state }

    let decompose = function
      | Value {prog; state} -> (prog, state)
      | Var (_,_) -> invalid_arg "Unexpected free variable"

    let inj t = to_logic (T.fmap P.inj S.inj t)

    let reify = reify P.reify S.reify

    let pprint =
      let pp ff { prog; state; } =
        Format.fprintf ff "(%a, %a)" P.pprint prog S.pprint state
      in
      Utils.pprint_logic pp

    let progo t prog =
      fresh (state)
        (t === cfg prog state)

    let stateo t state =
      fresh (prog)
        (t === cfg prog state)

    let lift_split splito t result =
      fresh (prog state result')
        (t === cfg prog state)
        (splito prog result')
        (conde [
          (result' === Split.undef ()) &&& (result === Split.undef ());
          fresh (ctx rdx)
            (result' === Split.split ctx rdx)
            (result === Split.split ctx (cfg rdx state));
        ])

    let lift_plug plugo ctx rdx term =
      fresh (prog state prog')
        (rdx  === cfg prog  state)
        (term === cfg prog' state)
        (plugo ctx prog prog')

    let lift_rule rule ctx t t' =
      fresh (label prog state prog' state')
        (t  === cfg prog  state )
        (t' === cfg prog' state')
        (rule label ctx prog prog')
        (S.transitiono label state state')

  end

type ('tt, 'cst, 'tl, 'csl) step =
  ('tt, 'tl) Term.ti -> ('tt, 'tl) MaybeTerm.ti -> goal

type ('tt, 'tl) eval =
  ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

let make_reduction_relation splito plugo rules = fun term result ->
  fresh (ctx_rdx)
    (splito term ctx_rdx)
    (conde [
      (ctx_rdx === Split.undef ()) &&& (result === MaybeTerm.undef ());
      fresh (ctx rdx rdx' term')
        (ctx_rdx === Split.split ctx rdx)
        (result === MaybeTerm.term term')
        (conde @@ List.map (fun rule -> rule ctx rdx rdx') rules)
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
  let evalo = ref (fun term term' -> assert false) in
  let evalo_rec term term' = evalo_norec !evalo term term' in
  let evalo_tabled = Tabling.(tabled two) evalo_rec in
  evalo := evalo_tabled;
  evalo_tabled t t'
