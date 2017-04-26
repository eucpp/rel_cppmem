module Loc :
  sig
    type tt = string
    type tl = string MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    val of_string : string -> tt
    val to_string : tt -> string
  end

module Var :
  sig
    type tt = string
    type tl = string MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected
  end

module Value :
  sig
    type tt = MiniKanren.Nat.ground
    type tl = MiniKanren.Nat.logic
    type ti = MiniKanren.Nat.groundi

    val of_string : string -> tt
    val to_string : tt -> string

    val to_logic : tt -> tl
  end

module Timestamp :
  sig
    type tt = MiniKanren.Nat.ground
    type tl = MiniKanren.Nat.logic
    type ti = MiniKanren.Nat.groundi
  end

module MemOrder :
  sig
    type tt = SC | ACQ | REL | ACQ_REL | CON | RLX | NA
    type tl = tt MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    val of_string : string -> tt
    val to_string : tt -> string
  end

module Path :
  sig
    module T :
      sig
        type 'a t = N | L of 'a | R of 'a
      end

    type tt = tt T.t
    type tl = tl T.t MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    val pathn : unit -> ti
    val pathl : ti -> ti
    val pathr : ti -> ti

    val inj : tt -> ti
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
        (* with gmap *)
      end

    (* include (module type of MiniKanren.Fmap5(T)) *)

    type tt  = (Value.tt, Var.tt, MemOrder.tt, Loc.tt, tt) T.t
    type tl  = (Value.tl, Var.tl, MemOrder.tl, Loc.tl, tl) T.t MiniKanren.logic
    type ti  = (tt, tl) MiniKanren.injected

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
    val inj_logic : MiniKanren.Mapping.t -> tl -> ti

    val to_logic   : tt -> tl
    val from_logic : tl -> tt

    val preallocate : tt -> Var.tt list * Loc.tt list

    val pprint : tl -> string
  end

module Context :
  sig
    module T :
      sig
        type ('expr, 'string, 'mo, 'loc, 't, 'c) t =
          | Hole
          | BinopL    of 'string * 'c * 't
          | BinopR    of 'string * 't * 'c
          | PairL     of 'c * 't
          | PairR     of 't * 'c
          | AsgnC     of 't * 'c
          | WriteC    of 'mo * 'loc * 'c
          | IfC       of 'c * 't * 't
          | SeqC      of 'c * 't
          | ParL      of 'c * 't
          | ParR      of 't * 'c
        (* with gmap *)
      end

    type tt  = (Value.tt, Var.tt, MemOrder.tt, Loc.tt, Term.tt, tt) T.t
    type tl  = (Value.tl, Var.tl, MemOrder.tl, Loc.tl, Term.tl, tl) T.t MiniKanren.logic
    type ti  = (tt, tl) MiniKanren.injected

    val inj : tt -> ti
  end

type tt  = Term.tt
type tl  = Term.tl
type ti  = Term.ti

type ct  = Context.tt
type cl  = Context.tl
type ci  = Context.ti

val reducibleo : ti -> MiniKanren.Bool.groundi -> MiniKanren.goal

val splito : ti -> ci -> ti -> MiniKanren.goal
val plugo  : ti -> ci -> ti -> MiniKanren.goal

val patho : ci -> Path.ti -> MiniKanren.goal
