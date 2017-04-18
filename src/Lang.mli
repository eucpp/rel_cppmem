module Loc :
  sig
    type tt = string
    type tl = string MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    val of_string : string -> tt
    val to_string : tt -> string
  end

module Value :
  sig
    type tt = MiniKanren.Nat.ground
    type tl = MiniKanren.Nat.logic
    type ti = MiniKanren.Nat.groundi

    val of_string : string -> tt
    val to_string : tt -> string
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
    type 'a t = N | L of 'a | R of 'a

    type tt = tt t
    type tl = tl t MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected
  end

val inj_path : Path.tt -> Path.ti

val pathn : Path.ti
val pathl : Path.ti -> Path.ti
val pathr : Path.ti -> Path.ti

module Term :
  sig
    @type ('int, 'string, 'mo, 'loc, 't) t =
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
    with gmap

    type tt  = (Value.tt, string, MemOrder.tt, Loc.tt, tt) t
    type tl  = (Value.tl, string MiniKanren.logic, MemOrder.tl, Loc.tl, tl) t MiniKanren.logic
    type ti  = (tt, tl) MiniKanren.injected

    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) ->
               ('a, 'b, 'c, 'd, 'e) t -> ('q, 'r, 's, 't, 'u) t

    val pprint : tt -> string
  end

module Context :
  sig
    @type ('expr, 'string, 'mo, 'loc, 't, 'c) t =
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
    with gmap

    type tt  = (Value.tt, string, MemOrder.tt, Loc.tt, Term.tt, tt) t
    type tl  = (Value.tl, string MiniKanren.logic, MemOrder.tl, Loc.tl, Term.tl, tl) t MiniKanren.logic
    type ti  = (tt, tl) MiniKanren.injected

    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('f -> 'v) ->
               ('a, 'b, 'c, 'd, 'e, 'f) t -> ('q, 'r, 's, 't, 'u, 'v) t
  end

type t   = Term.tt
type tl  = Term.tl
type ti  = Term.ti

type c   = Context.tt
type cl  = Context.tl
type ci  = Context.ti

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
val skip    : ti
val stuck   : ti

val preallocate : t -> string list * Loc.tt list

val inj_term : t -> ti
val inj_context : c -> ci

val reducibleo : ti -> MiniKanren.Bool.groundi -> MiniKanren.goal

val splito : ti -> ci -> ti -> MiniKanren.goal
val plugo  : ti -> ci -> ti -> MiniKanren.goal

val patho : ci -> Path.ti -> MiniKanren.goal
