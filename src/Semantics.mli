(** Term *)
module Term :
  sig
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.injected
  end

(** MaybeTerm - term augmented with undefined value *)
module MaybeTerm :
  sig
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.Option.groundi

    val term  : ('tt, 'tl) MiniKanren.injected -> ti
    val undef : unit -> ti
  end

(** Context - evaluation context *)
module Context :
  sig
    type ('ct, 'cl) ti = ('ct, 'cl) MiniKanren.injected
  end

(** Constraints - additional constraints which are required for some rule to fire *)
module Constraints :
  sig
    type ('cst, 'csl) ti = ('cst, 'csl) MiniKanren.injected
  end

(** Split - the result of splitting application, i.e. pair of evaluation context and term to be reduced (redex).
      Since some terms may be irreducible, the context/redex pair may be undefined *)
module Split :
  sig
    type ('t, 'c) tt
    type ('t, 'c) tl

    type ('tt, 'ct, 'tl, 'cl) ti = (('tt, 'tl) tt, ('ct, 'cl) tl) MiniKanren.injected

    val split : ('ct, 'cl) MiniKanren.injected -> ('tt, 'tl) MiniKanren.injected -> ('tt, 'ct, 'tl, 'cl) ti
    val undef : unit -> ('tt, 'ct, 'tl, 'cl) ti

    val redexo   : ('tt, 'ct, 'tl, 'cl) ti -> ('tt, 'tl) MiniKanren.injected -> MiniKanren.goal
    val contexto : ('tt, 'ct, 'tl, 'cl) ti -> ('ct, 'cl) MiniKanren.injected -> MiniKanren.goal
  end

(** [splitting term split] - either splits [term] into [split] = [(context, redex)], or binds irreducible [term] with [undef] *)
type ('tt, 'ct, 'tl, 'cl) splitting =
  ('tt, 'tl) Term.ti -> ('tt, 'ct, 'tl, 'cl) Split.ti -> goal

(** [plugging context redex term] - plugs [redex] into [context] obtaining new [term] *)
type ('tt, 'ct, 'tl, 'cl) plugging =
  ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> goal

(** [rule context constraints term term'] - substitutes [term] with [term'] in a [context] considering provided [constraints] *)
type ('tt, 'ct, 'cst, 'tl, 'cl, 'csl) rule =
  ('cst, 'csl) Constraints.ti -> ('ct, 'cl) Context.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> goal

(** [step constraints term term'] - performs a step that substitutes [term] with [term'] considering provided [constraints],
      if [term] is irreducible then [term'] is undefined *)
type ('tt, 'cst, 'tl, 'csl) step =
  ('cst, 'csl) Constraints.ti -> ('tt, 'tl) Term.ti -> ('tt, 'tl) MaybeTerm.ti -> goal

(** [eval term term'] - evaluates [term] to [term'] *)
type ('tt, 'tl) eval =
  ('tt, 'tl) Term.ti -> ('tt, 'tl) Term.ti -> goal

(** [make_reduction_relation splitting plugging rules] - given the [splitting], [plugging] relations and the set of [rules]
      returns the [step constraints term term'] relation that proceeds as follows:
        1) tries to split [term] into [(context, redex)] using [splitting] relation, if fails (i.e. [term] is irreducible) then [term'] is undefined
        2) applies each [rule] from list of [rules], i.e. [rule constraints context redex redex']
        3) plugs [redex'] back to [context] using [plugging] relation
  *)
val make_reduction_relation :
  ('tt, 'ct, 'tl, 'cl) splitting -> ('tt, 'ct, 'tl, 'cl) plugging -> ('tt, 'ct, 'cst, 'tl, 'cl, 'csl) rule list -> ('tt, 'ct, 'cst, 'tl, 'cl, 'csl) step

val make_eval :
  ('tt, 'cst, 'tl, 'csl) step -> ('tt, 'tl) eval
