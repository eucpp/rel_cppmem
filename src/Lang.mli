module Register :
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val reg : string -> ti

    val inj : tt -> ti

    val show : tl -> string
  end

module Loc :
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val loc : string -> ti

    val inj : tt -> ti

    val show : tl -> string
  end

module Value :
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val value : int -> ti

    val inj : tt -> ti

    val show : tl -> string
  end

module MemOrder :
  sig
    type tt = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val mo : string -> ti

    val inj : tt -> ti

    val show : tl -> string
  end

module Op :
  sig
    type tt = ADD | MUL | EQ | NEQ | LT | LE | GT | GE

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val op : string -> ti

    val inj : tt -> ti

    val show : tl -> string
  end

module Term :
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) Semantics.Term.ti

    val const   : Value.ti -> ti
    val var     : Loc.ti -> ti
    val binop   : Op.ti -> ti -> ti -> ti
    val asgn    : ti -> ti -> ti
    val pair    : ti -> ti -> ti
    val if'     : ti -> ti -> ti -> ti
    val repeat  : ti -> ti
    val read    : MemOrder.ti -> Loc.ti -> ti
    val write   : MemOrder.ti -> Loc.ti -> ti -> ti
    val cas     : MemOrder.ti -> MemOrder.ti -> Loc.ti -> ti -> ti -> ti
    val seq     : ti -> ti -> ti
    val spw     : ti -> ti -> ti
    val par     : ti -> ti -> ti
    val skip    : unit -> ti
    val stuck   : unit -> ti

    val inj : tt -> ti

    val to_logic   : tt -> tl
    val from_logic : tl -> tt

    val refine : (tt, tl) MiniKanren.refined -> tl

    val show : tl -> string
    val pprint : tl -> string
  end

module ThreadID :
  sig
    type tt
    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val pathn : unit -> ti
    val pathl : ti -> ti
    val pathr : ti -> ti

    val inj : tt -> ti
  end

module Context :
  sig
    module T :
      sig
        type ('t, 'path) t = {
          term : 't;
          hole : 't;
          path : 'path;
        }
      end

    type tt = (Term.tt, ThreadID.tt) T.t
    type tl = (Term.tl, ThreadID.tl) T.t MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    val patho : ti -> ThreadID.ti -> MiniKanren.goal
  end

val splito : Term.ti -> (Term.tt, Context.tt, Term.tl, Context.tl) Semantics.Split.ti -> MiniKanren.goal

val promiseo : Term.ti -> (Term.tt, Context.tt, Term.tl, Context.tl) Semantics.Split.ti -> MiniKanren.goal

val plugo : Context.ti -> Term.ti -> Term.ti -> MiniKanren.goal
