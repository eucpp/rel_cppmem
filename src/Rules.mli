module Constraints :
  sig
    type 'thrdId t

    type tt = Lang.ThreadID.tt t
    type tl = Lang.ThreadID.tl t MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val thrd_ido : ti -> Lang.ThreadID.ti -> MiniKanren.goal
  end

module RuleTypes (Machine : Machines.Sequential) :
  sig
    module CFG : module type of Semantics.Configuration(Lang.Term)(Machine)

    type tt = CFG.tt
    type tl = CFG.tl

    type ct = Lang.Context.tt
    type cl = Lang.Context.tl

    type cst = Constraints.tt
    type csl = Constraints.tl

    type rule = (tt, ct, cst, tl, cl, csl) Semantics.rule
  end

module Basic (Machine : Machines.Sequential) :
  sig
    include (module type of RuleTypes(Machine))

    val varo    : rule
    val binopo  : rule
    val asgno   : rule
    val ifo     : rule
    val repeato : rule
    val seqo    : rule

    val all : rule list
  end

module ThreadSpawning (Machine : Machines.Parallel) :
  sig
    include (module type of RuleTypes(Machine))

    val spawno  : rule
    val joino   : rule

    val all : rule list
  end

module NonAtomic (Machine : Machines.NonAtomic) :
  sig
    include (module type of RuleTypes(Machine))

    val load_nao  : rule
    val store_nao : rule

    val load_data_raceo  : rule
    val store_data_raceo : rule

    val all : rule list
  end

module SequentialConsistent (Machine : Machines.SequentialConsistent) :
  sig
    include (module type of RuleTypes(Machine))

    val load_sco  : rule
    val store_sco : rule

    val all : rule list
  end

module ReleaseAcquire (Machine : Machines.ReleaseAcquire) :
  sig
    include (module type of RuleTypes(Machine))

    val load_acqo  : rule
    val store_relo : rule

    val all : rule list
  end

(*
module Rlx :
  sig
    val read_rlx  : string * rule
    val write_rlx : string * rule

    val all : (string * rule) list

    module Step : CppMemStep
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
*)
