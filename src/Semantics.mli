(** Term *)
module Term :
  sig
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.injected
  end

(** MaybeTerm - term augmented with undefined value *)
module MaybeTerm :
  sig
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.Std.Option.groundi

    val term  : ('tt, 'tl) MiniKanren.injected -> ('tt, 'tl) ti
    val undef : unit -> ('tt, 'tl) ti
  end

(** Context - evaluation context *)
module Context :
  sig
    type ('ct, 'cl) ti = ('ct, 'cl) MiniKanren.injected
  end

(* Label - label for Transition Labeled Systems *)
module Label :
  sig
    type ('lt, 'll) ti = ('lt, 'll) MiniKanren.injected
  end

(** [tpred t] - some predicate defined on a set of terms *)
type ('tt, 'tl) tpred =
  ('tt, 'tl) Term.ti -> MiniKanren.goal

(** [splitting term ctx rdx] - splits [term] into pair [context] and [redex] *)
type ('tt, 'ct, 'tl, 'cl) splitting =
  ('tt, 'tl) Term.ti -> ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

(** [plugging context redex term] - plugs [redex] into [context] obtaining new [term] *)
type ('tt, 'ct, 'tl, 'cl) plugging =
  ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

(** [rule context term term'] - substitutes [term] with [term'] in a [context] *)
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

(** TLSNode - special case of [Term] for Transition Labeled System semantics.
  *   This kind of semantics can be viewed as graphs.
  *   Nodes of these graph denotes states of abstract machine,
  *   and (labeled) edges denotes possible transitions between state.
  *   [Term] of TLS consists of a program (that defines a set of transitions) and a current state of system.
  *)
module TLSNode (P : Utils.Logic) (S : State):
  sig
    include Utils.Logic

    type ('tt, 'ct, 'tl, 'cl) rule' = ('tt, 'ct, 'tl, 'cl) rule

    (** [rule constraints context prog state prog' state'] - special case of rule
      *   that operates on program terms and labels
      *)
    type ('ct, 'cl) rule =
      S.li -> ('ct, 'cl) Context.ti -> P.ti -> P.ti -> MiniKanren.goal

    val node : P.ti -> S.ti -> ti

    val decompose : tl -> P.tl * S.tl

    val progo  : ti -> P.ti -> MiniKanren.goal
    val stateo : ti -> S.ti -> MiniKanren.goal

    val lift_tpred :
      (P.tt, P.tl) tpred -> (tt, tl) tpred

    val lift_split :
      (P.tt, 'ct, P.tl, 'cl) splitting -> (tt, 'ct, tl, 'cl) splitting

    val lift_plug :
      (P.tt, 'ct, P.tl, 'cl) plugging -> (tt, 'ct, tl, 'cl) plugging

    val lift_rule :
      ('ct, 'cl) rule -> (tt, 'ct, tl, 'cl) rule'
  end

(** [step term term'] - performs a step that substitutes [term] with [term'] *)
type ('tt, 'tl) step =
  ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

(** [path term term'] - binds [term] and [term'] such that there exist
  *   a series of steps in semantics that reduces [term] to [term']
  *)
type ('tt, 'tl) path =
  ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

(** [eval ~irreducibleo term term'] - evaluates [term] to [term'], where [term'] is irreducible *)
type ('tt, 'tl) eval =
  ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> MiniKanren.goal

(** [make_reduction_relation splitting plugging rules] - given the [splitting], [plugging] relations and the set of [rules]
      returns the [step constraints term term'] relation that proceeds as follows:
        1) tries to split [term] into [(context, redex)] using [splitting] relation, if fails (i.e. [term] is irreducible) then [term'] is undefined
        2) applies each [rule] from list of [rules], i.e. [rule constraints context redex redex']
        3) plugs [redex'] back to [context] using [plugging] relation
  *)
val make_step :
  ('tt, 'ct, 'tl, 'cl) splitting -> ('tt, 'ct, 'tl, 'cl) plugging -> ('tt, 'ct, 'tl, 'cl) rule list -> ('tt, 'tl) step

val make_path :
  ('tt, 'tl) step -> ('tt, 'tl) path

val make_eval :
  irreducibleo:('tt, 'tl) tpred -> ('tt, 'tl) step -> ('tt, 'tl) eval
