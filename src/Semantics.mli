(** Term *)
module Term : module type of Utils.Injected

(** [tpred t] - some predicate defined on a set of terms *)
type ('tt, 'tl) tpred =
  ('tt, 'tl) Term.ti -> MiniKanren.goal

(** [eval term term'] - evaluates [term] to [term'] *)
type ('at, 'bt, 'al, 'bl) eval =
  ('at, 'al) Term.ti -> ('bt, 'bl) Term.ti -> MiniKanren.goal

(** Config - special case of term for languages that distinguish
  *   syntactic and semantic components.
  *   For these languages term is a pair consisting of AST-term/program and environment/state.
  *)
module type Config =
  sig
    include Utils.Logic

    (* program types *)
    type pt
    type pl = p_inner MiniKanren.logic
      and p_inner
    type pi = (pt, pl) MiniKanren.injected

    (* state types *)
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

module MakeConfig(P : Utils.Logic)(S : Utils.Logic) : Config with
      type pt       = P.tt
  and type p_inner  = P.inner
  and type pl       = P.tl
  and type pi       = P.ti
  and type st       = S.tt
  and type s_inner  = S.inner
  and type sl       = S.inner MiniKanren.logic
  and type si       = S.ti

(** Set of declarations for defining reduction small-step semantics *)
module Reduction :
  sig
    (** Context - evaluation context *)
    module Context :
      sig
        type ('ct, 'cl) ti = ('ct, 'cl) MiniKanren.injected
      end

    (** [splitting term ctx rdx] - splits [term] into pair [context] and [redex] *)
    type ('tt, 'ct, 'tl, 'cl) splitting =
      ('tt, 'tl) Term.ti -> ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

    (** [plugging context redex term] - plugs [redex] into [context] obtaining new [term] *)
    type ('tt, 'ct, 'tl, 'cl) plugging =
      ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

    (** [rule context term term'] - substitutes [term] with [term'] in a [context] *)
    type ('tt, 'ct, 'tl, 'cl) rule =
      ('ct, 'cl) Context.ti -> ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

    (** [step term term'] - performs a step that substitutes [term] with [term'] *)
    type ('tt, 'tl) step =
      ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

    (** [path term term'] - binds [term] and [term'] such that there exist
      *   a series of steps in semantics that reduces [term] to [term']
      *)
    type ('tt, 'tl) path =
      ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

    (** [make_reduction_relation splitting plugging rules] - given the [splitting], [plugging] relations and the set of [rules]
          returns the [step constraints term term'] relation that proceeds as follows:
            1) splits [term] into [(context, redex)] using [splitting] relation
            2) applies each [rule] from list of [rules], i.e. [rule context redex redex']
            3) plugs [redex'] back to [context] using [plugging] relation
      *)
    val make_step :
      ('tt, 'ct, 'tl, 'cl) splitting -> ('tt, 'ct, 'tl, 'cl) plugging -> ('tt, 'ct, 'tl, 'cl) rule list -> ('tt, 'tl) step

    val make_path :
      ('tt, 'tl) step -> ('tt, 'tl) path

    val make_eval :
      irreducibleo:('tt, 'tl) tpred -> ('tt, 'tl) step -> ('tt, 'tt, 'tl, 'tl) eval
  end

(** Prog *)
module Prog : module type of Utils.Injected

(** In *)
module Input : module type of Utils.Injected

(** Out *)
module Output : module type of Utils.Injected

type ('at, 'bt, 'ct, 'al, 'bl, 'cl) interpreter =
  ('at, 'al) Prog.ti -> ('bt, 'bl) Input.ti -> ('ct, 'cl) Output.ti -> MiniKanren.goal
