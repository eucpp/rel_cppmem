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

    type tt  = (Memory.Value.tt, Memory.Var.tt, Memory.MemOrder.tt, Memory.Loc.tt, tt) T.t
    type tl  = (Memory.Value.tl, Memory.Var.tl, Memory.MemOrder.tl, Memory.Loc.tl, tl) T.t MiniKanren.logic
    type ti  = (tt, tl) MiniKanren.injected

    val const   : Memory.Value.ti -> ti
    val var     : Memory.Loc.ti -> ti
    val binop   : Memory.Loc.ti -> ti -> ti -> ti
    val asgn    : ti -> ti -> ti
    val pair    : ti -> ti -> ti
    val if'     : ti -> ti -> ti -> ti
    val repeat  : ti -> ti
    val read    : Memory.MemOrder.ti -> Memory.Loc.ti -> ti
    val write   : Memory.MemOrder.ti -> Memory.Loc.ti -> ti -> ti
    val cas     : Memory.MemOrder.ti -> Memory.MemOrder.ti -> Memory.Loc.ti -> ti -> ti -> ti
    val seq     : ti -> ti -> ti
    val spw     : ti -> ti -> ti
    val par     : ti -> ti -> ti
    val skip    : unit -> ti
    val stuck   : unit -> ti

    val inj : tt -> ti

    val to_logic   : tt -> tl
    val from_logic : tl -> tt

    val refine : (tt, tl) MiniKanren.refined -> tl

    val preallocate : tl -> Memory.Var.tt list * Memory.Loc.tt list

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
          | SeqL      of 'c * 't
          | SeqR      of 't * 'c
          | ParL      of 'c * 't
          | ParR      of 't * 'c
        (* with gmap *)
      end

    type tt  = (Memory.Value.tt, Memory.Var.tt, Memory.MemOrder.tt, Memory.Loc.tt, Term.tt, tt) T.t
    type tl  = (Memory.Value.tl, Memory.Var.tl, Memory.MemOrder.tl, Memory.Loc.tl, Term.tl, tl) T.t MiniKanren.logic
    type ti  = (tt, tl) MiniKanren.injected

    val inj : tt -> ti

    val hole : unit -> ti

    val reducibleo : Term.ti -> MiniKanren.Bool.groundi -> MiniKanren.goal

    val splito : Term.ti -> ti -> Term.ti -> MiniKanren.goal
    val plugo  : Term.ti -> ti -> Term.ti -> MiniKanren.goal

    val patho : ti -> Memory.Path.ti -> MiniKanren.goal
  end

(* type tt  = Term.tt
type tl  = Term.tl
type ti  = Term.ti

type ct  = Context.tt
type cl  = Context.tl
type ci  = Context.ti

type st = Memory.MemState.tt
type sl = Memory.MemState.tl
type si = Memory.MemState.ti *)

type rule =  (Context.ti -> Term.ti -> Memory.MemState.ti ->
              Context.ti -> Term.ti -> Memory.MemState.ti -> MiniKanren.goal)

val make_reduction_relation : (string * rule) list -> (
  module Semantics.Step          with
    type tt = Term.tt            and
    type tl = Term.tl            and
    type st = Memory.MemState.tt and
    type sl = Memory.MemState.tl
)

val make_certified_relation : (string * rule) list -> (string * rule) list -> (
  module Semantics.Step          with
    type tt = Term.tt            and
    type tl = Term.tl            and
    type st = Memory.MemState.tt and
    type sl = Memory.MemState.tl
)
