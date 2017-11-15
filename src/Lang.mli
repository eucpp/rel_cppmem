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

    val inj : tt -> tl
    val reify : MiniKanren.helper -> ti -> tl

    val show : tl -> string

    val nullo     : ti -> MiniKanren.goal
    val not_nullo : ti -> MiniKanren.goal

    val addo : ti -> ti -> ti -> MiniKanren.goal
    val mulo : ti -> ti -> ti -> MiniKanren.goal

    val eqo  : ti -> ti -> MiniKanren.goal
    val neqo : ti -> ti -> MiniKanren.goal
    val lto  : ti -> ti -> MiniKanren.goal
    val leo  : ti -> ti -> MiniKanren.goal
    val gto  : ti -> ti -> MiniKanren.goal
    val geo  : ti -> ti -> MiniKanren.goal
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
    type tt = ADD | MUL | EQ | NEQ | LT | LE | GT | GE | OR | AND

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val op : string -> ti

    val inj : tt -> tl
    val reify : MiniKanren.helper -> ti -> tl

    val show : tl -> string
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

    val inj : tt -> tl

    val reify : MiniKanren.helper -> ti -> tl
  end

module Expr :
  sig
    include Utils.Logic

    val var       : Register.ti -> ti
    val const     : Value.ti -> ti
    val binop     : Op.ti -> ti -> ti -> ti

    val show : tl -> string
  end

module Term :
  sig
    include Utils.Logic

    val skip      : unit -> ti
    val stuck     : unit -> ti
    val assertion : Expr.ti -> ti
    val asgn      : Register.ti -> Expr.ti -> ti
    val if'       : Expr.ti -> ti -> ti -> ti
    val while'    : Expr.ti -> ti -> ti
    val load      : MemOrder.ti -> Loc.ti -> Register.ti -> ti
    val store     : MemOrder.ti -> Loc.ti -> Expr.ti     -> ti
    val cas       : MemOrder.ti -> MemOrder.ti -> Loc.ti -> Expr.ti -> Expr.ti -> ti
    val repeat    : MemOrder.ti -> Loc.ti -> ti
    val seq       : ti -> ti -> ti
    val spw       : ti -> ti -> ti
    val par       : ti -> ti -> ti

    val show : tl -> string

    val thrd_termo : ti -> ThreadID.ti -> ti -> MiniKanren.goal

    val thrd_local_termo : ti -> MiniKanren.goal
    val thrd_inter_termo : ti -> MiniKanren.goal

    val irreducibleo : ti -> MiniKanren.goal

    val bool_expro  : ?loco:(Loc.ti -> MiniKanren.goal) -> ti -> MiniKanren.goal
    val stmto       : ?loco:(Loc.ti -> MiniKanren.goal) -> ti -> MiniKanren.goal
    val seq_stmto   : ?loco:(Loc.ti -> MiniKanren.goal) -> ti -> MiniKanren.goal
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

module Label :
  sig
    include Utils.Logic

    val empty : unit -> ti

    val spawn : ThreadID.ti -> ti
    val join  : ThreadID.ti -> ti

    val regread  : ThreadID.ti -> Register.ti -> Value.ti -> ti
    val regwrite : ThreadID.ti -> Register.ti -> Value.ti -> ti

    val load  : ThreadID.ti -> MemOrder.ti -> Loc.ti -> Value.ti -> ti
    val store : ThreadID.ti -> MemOrder.ti -> Loc.ti -> Value.ti -> ti

    val datarace : ThreadID.ti -> MemOrder.ti -> Loc.ti -> ti

    val cas :
      ThreadID.ti -> MemOrder.ti -> MemOrder.ti -> Loc.ti ->
      Value.ti -> Value.ti -> Value.ti -> ti
  end

val splito : (Term.tt, Context.tt, Term.tl, Context.tl) Semantics.splitting

val thrd_splito : ThreadID.ti -> (Term.tt, Context.tt, Term.tl, Context.tl) Semantics.splitting

val plugo : (Term.tt, Context.tt, Term.tl, Context.tl) Semantics.plugging
