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

module type Config =
  sig
    include Utils.Logic

    type pt
    type pl = p_inner MiniKanren.logic
      and p_inner
    type pi = (pt, pl) MiniKanren.injected

    type st
    type sl = s_inner MiniKanren.logic
      and s_inner
    type si = (st, sl) MiniKanren.injected

    val cfg : pi -> si -> ti

    val decompose : tl -> pl * sl

    val progo  : ti -> pi -> MiniKanren.goal
    val stateo : ti -> si -> MiniKanren.goal
  end

module MakeConfig(P : Utils.Logic)(S : Utils.Logic) (*: Config with
      type pt       = P.tt
  and type p_inner  = P.inner
  and type pl       = P.tl
  and type pi       = P.ti
  and type st       = S.tt
  and type s_inner  = S.inner
  and type sl       = S.inner MiniKanren.logic
  and type si       = S.ti *)
  =
  struct
    type pt = P.tt
    type pl = p_inner MiniKanren.logic
      and p_inner = P.inner
    type pi = (pt, pl) MiniKanren.injected

    type st = S.tt
    type sl = s_inner MiniKanren.logic
      and s_inner = S.inner
    type si = (st, sl) MiniKanren.injected

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

    type tt = (pt, st) T.t
    type tl =  inner MiniKanren.logic
      and inner = (pl, sl) T.t
    type ti = (tt, tl) MiniKanren.injected

    let cfg prog state =
      inj @@ distrib @@ { prog; state }

    let decompose = function
      | Value {prog; state} -> (prog, state)
      | Var (_,_) -> invalid_arg "Unexpected free variable"

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

type ('tt, 'tl) tpred =
  ('tt, 'tl) Term.ti -> MiniKanren.goal

type ('tt, 'ct, 'tl, 'cl) splitting =
  ('tt, 'tl) Term.ti -> ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

type ('tt, 'ct, 'tl, 'cl) plugging =
  ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

type ('tt, 'ct, 'tl, 'cl) rule =
  ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

module type State =
  sig
    include Utils.Logic

    type lt

    type ll = linner MiniKanren.logic
      and linner

    type li = (lt, ll) MiniKanren.injected

    val transitiono : li -> ti -> ti -> MiniKanren.goal
  end

module TLSNode (P : Utils.Logic) (S : State) =
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

    type ('ct, 'cl) rule =
      S.li -> ('ct, 'cl) Context.ti -> P.ti -> P.ti -> MiniKanren.goal

    let node prog state =
      inj @@ distrib @@ { prog; state }

    let decompose = function
      | Value {prog; state} -> (prog, state)
      | Var (_,_) -> invalid_arg "Unexpected free variable"

    let reify = reify P.reify S.reify

    let pprint =
      let pp ff { prog; state; } =
        Format.fprintf ff "@[<v>Prog:@;<1 2>%a@;State:@;<1 2>%a@;@]" P.pprint prog S.pprint state
      in
      Utils.pprint_logic pp

    let progo t prog =
      fresh (state)
        (t === node prog state)

    let stateo t state =
      fresh (prog)
        (t === node prog state)

    let lift_tpred predo t =
      fresh (prog state)
        (t === node prog state)
        (predo prog)

    let lift_split splito t ctx rdx =
      fresh (prog state rdx')
        (t   === node prog  state)
        (rdx === node rdx' state)
        (splito prog ctx rdx')

    let lift_plug plugo ctx rdx term =
      fresh (prog state prog')
        (rdx  === node prog  state)
        (term === node prog' state)
        (plugo ctx prog prog')

    let lift_rule rule ctx t t' =
      fresh (label prog state prog' state')
        (t  === node prog  state )
        (t' === node prog' state')
        (rule label ctx prog prog')
        (S.transitiono label state state')

  end

type ('tt, 'tl) step =
  ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> goal

type ('tt, 'tl) path =
  ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

type ('tt, 'tl) eval =
  ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

let make_step splito plugo rules t t' =
  fresh (ctx rdx rdx')
    (splito t ctx rdx)
    (conde @@ List.map (fun rule -> rule ctx rdx rdx') rules)
    (plugo ctx rdx' t')

let make_path stepo =
  let patho_norec patho t t'' = conde [
    (t === t'');

    fresh (t')
      (stepo t t')
      (patho t' t'');
  ] in
  Tabling.(tabledrec two) patho_norec

let make_eval ~irreducibleo stepo =
  let evalo_norec evalo t t'' = conde [
    (irreducibleo t) &&& (t === t'');

  ?~(irreducibleo t) &&&
    fresh (t')
      (stepo t t')
      (evalo t' t'');
  ] in
  Tabling.(tabledrec two) evalo_norec
