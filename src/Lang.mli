module Loc :
  sig
    type tt = string
    type tl = string MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    val of_string : string -> tt
    val to_string : tt -> string

    val to_logic : tt -> tl

    val show : tl -> string
  end

module Var :
  sig
    type tt = string
    type tl = string MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    val of_string : string -> tt
    val to_string : tt -> string

    val inj : tt -> ti

    val to_logic : tt -> tl

    val show : tl -> string
  end

module Value :
  sig
    type tt = MiniKanrenStd.Nat.ground
    type tl = MiniKanrenStd.Nat.logic
    type ti = MiniKanrenStd.Nat.groundi

    val of_string : string -> tt
    val to_string : tt -> string

    val inj : tt -> ti

    val to_logic : tt -> tl

    val show : tl -> string
  end

module MemOrder :
  sig
    type tt = SC | ACQ | REL | ACQ_REL | CON | RLX | NA
    type tl = tt MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    val of_string : string -> tt
    val to_string : tt -> string

    val inj : tt -> ti

    val show : tl -> string
  end

module Term :
  sig
    module T :
      sig
        type ('int, 'string, 'mo, 'loc, 't) t =
          | Const    of 'int
          | Var      of 'string
          | Binop    of 'string * 't * 't
          | Asgn     of 't * 't
          | Pair     of 't * 't
          | If       of 't * 't * 't
          | Repeat   of 't
          | Read     of 'mo * 'loc
          | Write    of 'mo * 'loc * 't
          | Cas      of 'mo * 'mo * 'loc * 't * 't
          | Seq      of 't * 't
          | Spw      of 't * 't
          | Par      of 't * 't
          | Skip
          | Stuck
        (* with gmap, show *)
      end

    (* include (module type of MiniKanren.Fmap5(T)) *)

    type tt = (Value.tt, Var.tt, MemOrder.tt, Loc.tt, tt) T.t
    type tl = (Value.tl, Var.tl, MemOrder.tl, Loc.tl, tl) T.t MiniKanren.logic
    type ti = (tt, tl) Semantics.Term.ti

    val const   : Value.ti -> ti
    val var     : Loc.ti -> ti
    val binop   : Loc.ti -> ti -> ti -> ti
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
