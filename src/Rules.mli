module Constraints :
  sig
    type 'thrdId t

    type tt = Memory.ThreadID.tt t
    type tl = Memory.ThreadID.tl t MiniKanren.logic

    type ti = (tt, tl) MiniKanren.ti

    val thrd_ido : ti -> Memory.ThreadID.ti -> goal
  end

module Basic (Machine : Machines.Sequential) :
  sig
    type tt = (Lang.Term.tt, Machine.tt) Semantics.Configuration.tt
    type tl = (Lang.Term.tl, Machine.tl) Semantics.Configuration.tl

    type ct = Lang.Context.tt
    type cl = Lang.Context.tl

    type cst = Constraints.tt
    type csl = Constraints.tl

    type rule = (tt, ct, cst, tl, cl, csl) Semantics.rule

    val varo    : rule
    val binopo  : rule
    val asgno   : rule
    val ifo     : rule
    val repeato : rule
    val seqo    : rule

    val all : rule list
  end

(*
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
*)
