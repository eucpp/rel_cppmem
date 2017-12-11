open MiniKanren
open MiniKanren.Std

module Term = Utils.Injected

type ('tt, 'tl) tpred =
  ('tt, 'tl) Term.ti -> MiniKanren.goal

type ('at, 'bt, 'al, 'bl) eval =
  ('at, 'al) Term.ti -> ('bt, 'bl) Term.ti -> MiniKanren.goal

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

    val lift_tpred : (pt, pl) tpred -> (tt, tl) tpred
  end

module MakeConfig(P : Utils.Logic)(S : Utils.Logic) =
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
        Format.fprintf ff "@[<v>Prog:@;<1 2>%a@;State:@;<1 2>%a@;@]" P.pprint prog S.pprint state
      in
      Utils.pprint_logic pp

    let progo t prog =
      fresh (state)
        (t === cfg prog state)

    let stateo t state =
      fresh (prog)
        (t === cfg prog state)

    let lift_tpred predo t =
      fresh (prog state)
        (t === cfg prog state)
        (predo prog)
  end

module Reduction =
  struct
    module Context =
      struct
        type ('ct, 'cl) ti = ('ct, 'cl) MiniKanren.injected
      end

    type ('tt, 'ct, 'tl, 'cl) splitting =
      ('tt, 'tl) Term.ti -> ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

    type ('tt, 'ct, 'tl, 'cl) plugging =
      ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

    type ('tt, 'ct, 'tl, 'cl) rule =
      ('ct, 'cl) Context.ti -> ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

    type ('tt, 'tl) step =
      ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> goal

    type ('tt, 'tl) path =
      ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

    let make_step splito plugo rules t t' =
      fresh (ctx ctx' rdx rdx')
        (splito t ctx rdx)
        (plugo ctx' rdx' t')
        (conde @@ List.map (fun rule -> rule ctx ctx' rdx rdx') rules)

    let make_path stepo =
      let patho_norec patho t t'' = conde [
        (t === t'');

        fresh (t')
          (stepo t t')
          (patho t' t'');
      ] in
      Tabling.(tabledrec two) patho_norec

    let make_eval ~irreducibleo stepo =
      let evalo_norec evalo t t'' =
        (* fresh (t')
          (ifte ~cond:(stepo t t')
            ~th:(evalo t' t'')
            ~el:(t === t'')
          ) *)
      conde [
        (irreducibleo t) &&& (t === t'');

        fresh (t')
        (* ?~(irreducibleo t) *)
          (stepo t t')
          (evalo t' t'');
      ]
      in
      Tabling.(tabledrec two) evalo_norec
  end


(** Prog *)
module Prog = Utils.Injected

(** In *)
module Input = Utils.Injected

(** Out *)
module Output = Utils.Injected

type ('at, 'bt, 'ct, 'al, 'bl, 'cl) interpreter =
  ('at, 'al) Prog.ti -> ('bt, 'bl) Input.ti -> ('ct, 'cl) Output.ti -> MiniKanren.goal
