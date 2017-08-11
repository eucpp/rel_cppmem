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

    type tt = (Memory.Value.tt, Memory.Var.tt, Memory.MemOrder.tt, Memory.Loc.tt, tt) T.t
    type tl = (Memory.Value.tl, Memory.Var.tl, Memory.MemOrder.tl, Memory.Loc.tl, tl) T.t MiniKanren.logic
    type ti = (tt, tl) Semantics.Term.ti

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

    val show : tl -> string
    val pprint : tl -> string
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

    type tt = (Term.tt, Memory.Path.tt) T.t
    type tl = (Term.tl, Memory.Path.tl) T.t MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    val patho : ti -> Memory.Path.ti -> MiniKanren.goal
  end

val splito : Term.ti -> (Term.tt, Term.tl, Context.tt, Context.tl) Semantics.Split.ti -> MiniKanren.goal

val promiseo : Term.ti -> (Term.tt, Term.tl, Context.tt, Context.tl) Semantics.Split.ti -> MiniKanren.goal

val plugo : Context.ti -> Term.ti -> Term.ti -> MiniKanren.goal
