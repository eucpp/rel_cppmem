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

    val fst : ti
  end

module RegStorage :
  sig
    include Utils.Logic

    val empty : unit -> ti

    val allocate : Reg.ti list -> ti

    val from_assoc : (string * int) list -> ti

    val reado  : ti ->       Reg.ti -> Value.ti -> MiniKanren.goal
    val writeo : ti -> ti -> Reg.ti -> Value.ti -> MiniKanren.goal
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

module Prog :
  sig
    include Utils.Logic
  end

module Stmt :
  sig
    include Utils.Logic

    (* val skip      : unit -> ti *)
    val assertion : Expr.ti -> ti
    val asgn      : Reg.ti -> Expr.ti -> ti
    val if'       : Expr.ti -> Prog.ti -> Prog.ti -> ti
    val while'    : Expr.ti -> Prog.ti -> ti
    val repeat    : Prog.ti -> Expr.ti -> ti
    val load      : MemOrder.ti -> Loc.ti -> Reg.ti -> ti
    val store     : MemOrder.ti -> Loc.ti -> Expr.ti     -> ti
    val cas       : MemOrder.ti -> MemOrder.ti -> Loc.ti -> Expr.ti -> Expr.ti -> ti
    (* val seq       : ti -> ti -> ti *)
    val spw       : Prog.ti -> Prog.ti -> ti
    (* val par       : ti -> ti -> ti *)
    val return    : (Reg.tt, Reg.tl) MiniKanren.Std.List.groundi -> ti

    val show : tl -> string
  end

val prog : Stmt.ti list -> Prog.ti

module Error :
  sig
    include Utils.Logic

    val assertion : Expr.ti -> ti
    val datarace  : MemOrder.ti -> Loc.ti -> ti
  end

module Label :
  sig
    include Utils.Logic

    val empty : unit -> ti

    val spawn  : ThreadID.ti -> ThreadID.ti -> ti
    val join   : ThreadID.ti -> ThreadID.ti -> ti

    val load  : MemOrder.ti -> Loc.ti -> Value.ti -> ti
    val store : MemOrder.ti -> Loc.ti -> Value.ti -> ti

    val cas : MemOrder.ti -> MemOrder.ti -> Loc.ti -> Value.ti -> Value.ti -> Value.ti -> ti

    val error : Error.ti -> ti
  end

module Thread :
  sig
    include Utils.Logic

    val init : prog:Prog.ti -> regs:RegStorage.ti -> pid:ThreadID.ti -> ti
  end

module ThreadManager :
  sig
    include Utils.Logic

    val init : Thread.ti list -> ti

    val terminatedo : ti -> MiniKanren.goal

    val stepo : ThreadID.ti -> Label.ti -> ti -> ti -> MiniKanren.goal

    val intrpo : (Prog.tt, RegStorage.tt, RegStorage.tt, Prog.tl, RegStorage.tl, RegStorage.tl) Semantics.interpreter
  end
