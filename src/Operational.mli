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

module MemoryModel :
  sig
    module type T =
      sig
        include Utils.Logic

        val alloc : thrdn:int -> string list -> ti
        val init  : thrdn:int -> (string * int) list -> ti

        val checko : ti -> (string * int) list -> MiniKanren.goal

        val stepo : Lang.ThreadID.ti -> Lang.Label.ti -> ti -> ti -> MiniKanren.goal
      end
  end

module SequentialConsistent : MemoryModel.T

module ReleaseAcquire : MemoryModel.T

module Interpreter(Memory : MemoryModel.T) :
  sig
    module State :
      sig
        include Utils.Logic

        val init : Lang.ThreadManager.ti -> Memory.ti -> ti

        val memo : ti -> Memory.ti -> MiniKanren.goal

        val thrdmgro : ti -> Lang.ThreadManager.ti -> MiniKanren.goal

        val erroro :
         ?sg:(Lang.Error.ti -> MiniKanren.goal) ->
         ?fg:MiniKanren.goal ->
         ti -> MiniKanren.goal

        val safeo     : ti -> MiniKanren.goal
        val dataraceo : ti -> MiniKanren.goal
      end

    val stepo : State.ti -> State.ti -> MiniKanren.goal

    val reachableo : State.ti -> State.ti -> MiniKanren.goal

    val evalo : State.ti -> State.ti -> MiniKanren.goal

    val invarianto : (State.ti -> MiniKanren.goal) -> State.ti -> MiniKanren.goal

  end
