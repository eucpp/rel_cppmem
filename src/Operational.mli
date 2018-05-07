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

module type MemoryModel =
  sig
    include Utils.Logic

    val name : string

    val init  : thrdn:int -> (string * int) list -> ti

    val checko : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val terminatedo : ti -> MiniKanren.goal

    val stepo : Lang.ThreadID.ti -> Lang.Action.ti -> ti -> ti -> MiniKanren.goal
  end

module SeqCst : MemoryModel

module TSO : MemoryModel

module RelAcq : MemoryModel

module Interpreter(Memory : MemoryModel) :
  sig
    module State :
      sig
        include Utils.Logic

        val istate : Lang.ThreadManager.ti -> Memory.ti -> ti

        val init_istate  : regs:(string list) -> mem:((string * int) list) -> Lang.Prog.ti list -> ti
        val alloc_istate : regs:(string list) -> locs:(string list) -> Lang.Prog.ti list -> ti

        (* instantiate all `unknown` memory order specifiers to fresh variables *)
        val instmo : ti -> ti -> MiniKanren.goal

        val memo : ti -> Memory.ti -> MiniKanren.goal

        val thrdmgro : ti -> Lang.ThreadManager.ti -> MiniKanren.goal

        val erroro :
         ?sg:(Lang.Error.ti -> MiniKanren.goal) ->
         ?fg:MiniKanren.goal ->
         ti -> MiniKanren.goal

        val safeo     : ti -> MiniKanren.goal
        val dataraceo : ti -> MiniKanren.goal

        val terminatedo : ti -> MiniKanren.goal

        val sato : Lang.Prop.ti -> ti -> MiniKanren.goal
      end

    val stepo : State.ti -> State.ti -> MiniKanren.goal

    val reachableo : prop:Lang.Prop.ti -> State.ti -> State.ti -> MiniKanren.goal

    val reachable : prop:Lang.Prop.ti -> State.ti -> State.ri MiniKanren.Stream.t

    val evalo : ?prop:Lang.Prop.ti -> State.ti -> State.ti -> MiniKanren.goal

    val eval : ?prop:Lang.Prop.ti -> State.ti -> State.ri MiniKanren.Stream.t

    val invarianto : prop:Lang.Prop.ti -> State.ti -> MiniKanren.goal

    val invariant : prop:Lang.Prop.ti -> State.ti -> bool

  end
