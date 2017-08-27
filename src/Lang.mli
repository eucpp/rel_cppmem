module Register :
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val reg : string -> ti

    val inj : tt -> tl
    val reify : MiniKanren.helper -> ti -> tl

    val show : tl -> string
  end

module Loc :
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val loc : string -> ti

    val inj : tt -> tl
    val reify : MiniKanren.helper -> ti -> tl

    val show : tl -> string
  end

module Value :
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val integer : int -> ti

    val zero : unit -> ti
    val succ : ti -> ti

    val inj : tt -> tl
    val reify : MiniKanren.helper -> ti -> tl

    val show : tl -> string

    val addo : ti -> ti -> ti -> MiniKanren.goal
    val mulo : ti -> ti -> ti -> MiniKanren.goal

    val eqo : ti -> ti -> MiniKanren.Bool.groundi -> MiniKanren.goal
    val lto : ti -> ti -> MiniKanren.Bool.groundi -> MiniKanren.goal
    val leo : ti -> ti -> MiniKanren.Bool.groundi -> MiniKanren.goal
    val gto : ti -> ti -> MiniKanren.Bool.groundi -> MiniKanren.goal
    val geo : ti -> ti -> MiniKanren.Bool.groundi -> MiniKanren.goal
  end

module MemOrder :
  sig
    type tt = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val mo : string -> ti

    val inj : tt -> tl
    val reify : MiniKanren.helper -> ti -> tl

    val show : tl -> string
  end

module Op :
  sig
    type tt = ADD | MUL | EQ | NEQ | LT | LE | GT | GE

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val op : string -> ti

    val inj : tt -> tl
    val reify : MiniKanren.helper -> ti -> tl

    val show : tl -> string
  end

module Term :
  sig
    include Utils.Logic

    val const   : Value.ti -> ti
    val var     : Register.ti -> ti
    val binop   : Op.ti -> ti -> ti -> ti
    val asgn    : ti -> ti -> ti
    val pair    : ti -> ti -> ti
    val if'     : ti -> ti -> ti -> ti
    val while'  : ti -> ti -> ti
    val repeat  : ti -> ti
    val read    : MemOrder.ti -> Loc.ti -> ti
    val write   : MemOrder.ti -> Loc.ti -> ti -> ti
    val cas     : MemOrder.ti -> MemOrder.ti -> Loc.ti -> ti -> ti -> ti
    val seq     : ti -> ti -> ti
    val spw     : ti -> ti -> ti
    val par     : ti -> ti -> ti
    val skip    : unit -> ti
    val stuck   : unit -> ti

    val show : tl -> string

    val bool_expro  : ti -> MiniKanren.goal
    val stmto       : ti -> MiniKanren.goal
    val seq_stmto   : ti -> MiniKanren.goal
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

    val reify : MiniKanren.helper -> ti -> tl
  end

module Context :
  sig
    module T :
      sig
        type ('t, 'thrdId) t = {
          term : 't;
          hole : 't;
          thrdId : 'thrdId;
        }
      end

    type tt = (Term.tt, ThreadID.tt) T.t
    type tl = (Term.tl, ThreadID.tl) T.t MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    val thrdIdo : ti -> ThreadID.ti -> MiniKanren.goal
  end

val splito : Term.ti -> (Term.tt, Context.tt, Term.tl, Context.tl) Semantics.Split.ti -> MiniKanren.goal

val promiseo : Term.ti -> (Term.tt, Context.tt, Term.tl, Context.tl) Semantics.Split.ti -> MiniKanren.goal

val plugo : Context.ti -> Term.ti -> Term.ti -> MiniKanren.goal
