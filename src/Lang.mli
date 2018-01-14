module Reg :
  sig
    include Utils.Logic

    val reg : string -> ti

    val show : tl -> string
  end

module Loc :
  sig
    include Utils.Logic

    val loc : string -> ti

    val show : tl -> string
  end

module Value :
  sig
    include Utils.Logic

    val integer : int -> ti

    val show : tl -> string

    val nullo     : ti -> MiniKanren.goal
    val not_nullo : ti -> MiniKanren.goal

    val addo : ti -> ti -> ti -> MiniKanren.goal
    val mulo : ti -> ti -> ti -> MiniKanren.goal

    val eqo : ti -> ti -> MiniKanren.goal
    val nqo : ti -> ti -> MiniKanren.goal
    val lto : ti -> ti -> MiniKanren.goal
    val leo : ti -> ti -> MiniKanren.goal
    val gto : ti -> ti -> MiniKanren.goal
    val geo : ti -> ti -> MiniKanren.goal
  end

module MemOrder :
  sig
    type tt = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val mo : string -> ti

    val reify : MiniKanren.helper -> ti -> tl

    val show : tl -> string
  end

module Uop :
  sig
    type tt = NOT

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val uop : string -> ti

    val reify : MiniKanren.helper -> ti -> tl

    val show : tl -> string
  end

module Bop :
  sig
    type tt = ADD | MUL | EQ | NEQ | LT | LE | GT | GE | OR | AND

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val bop : string -> ti

    val reify : MiniKanren.helper -> ti -> tl

    val show : tl -> string
  end

module ThreadID :
  sig
    include Utils.Logic
  end

module Expr :
  sig
    include Utils.Logic

    val var       : Reg.ti -> ti
    val const     : Value.ti -> ti
    val unop      : Uop.ti -> ti -> ti
    val binop     : Bop.ti -> ti -> ti -> ti

    val show : tl -> string
  end

module Stmt :
  sig
    include Utils.Logic

    type progi = (tt, tl) MiniKanren.Std.List.groundi

    (* val skip      : unit -> ti *)
    val assertion : Expr.ti -> ti
    val asgn      : Reg.ti -> Expr.ti -> ti
    val if'       : Expr.ti -> progi -> progi -> ti
    val while'    : Expr.ti -> progi -> ti
    val repeat    : progi -> Expr.ti -> ti
    val load      : MemOrder.ti -> Loc.ti -> Reg.ti -> ti
    val store     : MemOrder.ti -> Loc.ti -> Expr.ti     -> ti
    val cas       : MemOrder.ti -> MemOrder.ti -> Loc.ti -> Expr.ti -> Expr.ti -> ti
    (* val seq       : ti -> ti -> ti *)
    val spw       : progi -> progi -> ti
    (* val par       : ti -> ti -> ti *)
    val return    : (Reg.tt, Reg.tl) MiniKanren.Std.List.groundi -> ti

    val show : tl -> string
  end

module Label :
  sig
    include Utils.Logic

    val empty : unit -> ti

    (* val spawn  : ThreadID.ti -> ti
    val join   : ThreadID.ti -> ti *)

    val spawn  : (ThreadID.tt, ThreadID.tl) MiniKanren.Std.List.groundi -> ti
    val join   : (ThreadID.tt, ThreadID.tl) MiniKanren.Std.List.groundi -> ti

    val load  : MemOrder.ti -> Loc.ti -> Value.ti -> ti
    val store : MemOrder.ti -> Loc.ti -> Value.ti -> ti

    val cas : MemOrder.ti -> MemOrder.ti -> Loc.ti -> Value.ti -> Value.ti -> Value.ti -> ti

    val datarace : MemOrder.ti -> Loc.ti -> ti

    val assert_fail : unit -> ti
  end

(* module RegStorage :
  sig
    include Utils.Logic

    val empty : unit -> ti

    val allocate : Register.ti list -> ti

    val from_assoc : (Register.ti * Value.ti) list -> ti

    val reado  : ti ->       Register.ti -> Value.ti -> MiniKanren.goal
    val writeo : ti -> ti -> Register.ti -> Value.ti -> MiniKanren.goal

    val spawno : ti -> ti -> ti -> MiniKanren.goal

    val joino  : ti -> ti -> ti -> ti -> MiniKanren.goal
  end *)
(*
module ThreadSubSys :
  sig
    include Utils.Logic

    val step : Label.ti -> ti -> ti -> MiniKanren.goal
    val thrdstep : ThreadId.ti -> Label.ti -> ti -> ti -> MiniKanren.goal
  end *)
