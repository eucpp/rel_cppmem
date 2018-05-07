(* Copyright (c) 2016-2018
 * Evgenii Moiseenko and Anton Podkopaev
 * St.Petersburg State University, JetBrains Research
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

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

    val zero  : ti
    val one   : ti

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
    type tt = SC | ACQ | REL | ACQ_REL | CON | RLX | NA | UNKW

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val mo : string -> ti

      val reify : MiniKanren.Env.t -> ti -> tl

    val show : tl -> string
  end

module Regs :
  sig
    include Utils.Logic

    val empty : unit -> ti

    val alloc : string list -> ti
    val init : (string * int) list -> ti

    val reado  : ti ->       Reg.ti -> Value.ti -> MiniKanren.goal
    val writeo : ti -> ti -> Reg.ti -> Value.ti -> MiniKanren.goal

    val checko : ti -> (string * int) list -> MiniKanren.goal
  end

module Uop :
  sig
    type tt

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val uop : string -> ti

    val reify : MiniKanren.Env.t -> ti -> tl

    val show : tl -> string
  end

module Bop :
  sig
    type tt

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    val bop : string -> ti

    val reify : MiniKanren.Env.t -> ti -> tl

    val show : tl -> string
  end

module ThreadID :
  sig
    include Utils.Logic

    val tid : int -> ti

    val null : ti
    val init : ti
  end

module Expr :
  sig
    include Utils.Logic

    val var       : Reg.ti -> ti
    val const     : Value.ti -> ti
    val unop      : Uop.ti -> ti -> ti
    val binop     : Bop.ti -> ti -> ti -> ti
    val choice    : ti -> ti -> ti

    val show : tl -> string
  end

module Prog :
  sig
    include Utils.Logic

    val instmo : ti -> ti -> MiniKanren.goal
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
    val cas       : MemOrder.ti -> MemOrder.ti -> Loc.ti -> Expr.ti -> Expr.ti -> Reg.ti -> ti

    val skip    : unit -> Prog.ti
    val single  : ti -> Prog.ti

    val show : tl -> string
  end

module CProg :
  sig
    include Utils.Logic
  end

val prog  : Stmt.ti list -> Prog.ti
val cprog : Prog.ti list -> CProg.ti

val thrdnum : CProg.ti -> int

module Error :
  sig
    include Utils.Logic

    val assertion : Expr.ti -> ti
    val datarace  : MemOrder.ti -> Loc.ti -> ti
  end

module Action :
  sig
    include Utils.Logic

    val eps : unit -> ti

    val r : MemOrder.ti -> Loc.ti -> Value.ti -> ti
    val w : MemOrder.ti -> Loc.ti -> Value.ti -> ti

    val rmw : MemOrder.ti -> Loc.ti -> Value.ti -> Value.ti -> ti

    val err : Error.ti -> ti

    val erroro :
      ?sg:(Error.ti -> MiniKanren.goal) ->
      ?fg:MiniKanren.goal ->
      ti -> MiniKanren.goal
  end

module Prop :
  sig
    include Utils.Logic

    val true_   : unit -> ti
    val false_  : unit -> ti

    val reg_eq : ThreadID.ti -> Reg.ti -> Value.ti -> ti
    val loc_eq : Loc.ti -> Value.ti -> ti

    type lhs

    val loc  : string -> lhs
    val (%)  : int -> string -> lhs
    val (=)  : lhs -> int -> ti

    val conj : ti -> ti -> ti
    val disj : ti -> ti -> ti
    val neg  : ti -> ti

    val (&&) : ti -> ti -> ti
    val (||) : ti -> ti -> ti
    val (!)  : ti -> ti

    val datarace  : unit -> ti
    val assertion : unit -> ti
  end

module ThreadLocalStorage(T : Utils.Logic) :
  sig
    include Utils.Logic

    val init : int -> T.ti -> ti
    val initi : int -> (ThreadID.ti -> T.ti) -> ti
    val of_list : T.ti list -> ti

    val geto : ti       -> ThreadID.ti -> T.ti -> MiniKanren.goal
    val seto : ti -> ti -> ThreadID.ti -> T.ti -> MiniKanren.goal

    val tidso : ti -> (ThreadID.tt, ThreadID.tl) MiniKanren.Std.List.groundi -> MiniKanren.goal

    val mapo : (T.ti -> T.ti -> MiniKanren.goal) -> ti -> ti -> MiniKanren.goal

    val foldo :
      (T.ti -> ('at, _ MiniKanren.logic as 'al) MiniKanren.injected -> ('at, 'al) MiniKanren.injected -> MiniKanren.goal) ->
      ti -> ('at, 'al) MiniKanren.injected -> ('at, 'al) MiniKanren.injected -> MiniKanren.goal

    val forallo : (T.ti -> MiniKanren.goal) -> ti -> MiniKanren.goal
  end

module Thread :
  sig
    include Utils.Logic

    val init : ?pid:ThreadID.ti -> Prog.ti -> Regs.ti -> ti

    val progo : ti -> Prog.ti -> MiniKanren.goal
    val regso : ti -> Regs.ti -> MiniKanren.goal

    val instmo : ti -> ti -> MiniKanren.goal

    val terminatedo : ti -> MiniKanren.goal
  end

module ThreadManager :
  sig
    include module type of ThreadLocalStorage(Thread)

    val init : Prog.ti list -> Regs.ti list -> ti

    val cprogo : ti -> CProg.ti -> MiniKanren.goal

    val instmo : ti -> ti -> MiniKanren.goal

    val terminatedo : ti -> MiniKanren.goal

    val stepo : ThreadID.ti -> Action.ti -> ti -> ti -> MiniKanren.goal

    val non_silent_stepo : ThreadID.ti -> Action.ti -> ti -> ti -> MiniKanren.goal
  end

module SeqProg :
  sig
    module Result :
      sig
        include Utils.Logic

        val regso : ti -> Regs.ti -> MiniKanren.goal

        val erroro :
          ?sg:(Error.ti -> MiniKanren.goal) ->
          ?fg:MiniKanren.goal ->
          ti -> MiniKanren.goal
      end

    val evalo : Prog.ti -> Regs.ti -> Result.ti -> MiniKanren.goal
  end
