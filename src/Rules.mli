
type ti = Lang.Term.ti
type ci = Lang.Context.ti
type si = Memory.MemState.ti

type rule =  (ci -> ti -> si -> ti -> si -> MiniKanren.goal)

type condition = (ci -> ti -> si -> MiniKanren.goal)

type predicate = (ti * si -> MiniKanrenStd.Bool.groundi -> MiniKanren.goal)

type order = (ti -> Lang.Decay.ti -> MiniKanren.goal)

module type CppMemStep = Semantics.StepRelation with
  type tt = Lang.Term.tt       and
  type tl = Lang.Term.tl       and
  type st = Memory.MemState.tt and
  type sl = Memory.MemState.tl

val make_reduction_relation :
  ?preconditiono:condition  ->
  ?postconditiono:condition ->
  ?ordero:order             ->
  (string * rule) list      ->
  (module CppMemStep)

module Basic :
  sig
    val var    : string * rule
    val binop  : string * rule
    val asgn   : string * rule
    val if'    : string * rule
    val repeat : string * rule
    val seq    : string * rule

    val all : (string * rule) list

    module Step : CppMemStep
  end

module ThreadSpawning :
  sig
    val spawn  : string * rule
    val join   : string * rule

    val all : (string * rule) list

    module Step : CppMemStep
  end

module NonAtomic :
  sig
    val read_na  : string * rule
    val write_na : string * rule

    val read_na_dr  : string * rule
    val write_na_dr : string * rule

    val read_dr  : string * rule
    val write_dr : string * rule

    val all : (string * rule) list
  end

module Rlx :
  sig
    val read_rlx  : string * rule
    val write_rlx : string * rule

    val all : (string * rule) list

    module Step : CppMemStep
  end

module RelAcq :
  sig
    val read_acq  : string * rule
    val write_rel : string * rule

    val all : (string * rule) list

    module Step : CppMemStep
  end

module SC :
  sig
    val read_sc  : string * rule
    val write_sc : string * rule

    val all : (string * rule) list
  end

module Promising :
  sig
    val promise : string * rule
    val fulfill : string * rule

    val all : (string * rule) list

    module PromiseStep : CppMemStep
    module FulfillStep : CppMemStep

    val make_certified_step : (string * rule) list -> (module CppMemStep)
  end
