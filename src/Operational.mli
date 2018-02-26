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

    val alloc : thrdn:int -> string list -> ti

    val init  : thrdn:int -> (string * int) list -> ti

    val checko : ti -> (string * int) list -> MiniKanren.goal

    val stepo : Lang.ThreadID.ti -> Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end

module Tactic :
  sig
    type t =
      | SingleThread of Lang.ThreadID.ti
      | Sequential
      | Interleaving
  end

module SequentialInterpreter :
  sig
    module State :
      sig
        include Utils.Logic

        val init : Lang.Regs.ti -> ti

        val regso : ?err:Lang.Error.ti -> ti -> Lang.Regs.ti -> MiniKanren.goal

        val safeo : ti -> MiniKanren.goal

        val erroro :
          ?sg:(Lang.Error.ti -> MiniKanren.goal) ->
          ?fg:MiniKanren.goal ->
          ti -> MiniKanren.goal
      end

    module ProgramState :
      sig
        include Utils.Logic

        val make : Lang.Prog.ti -> State.ti -> ti

        val stateo : ti -> State.ti -> MiniKanren.goal

        val terminatedo : ti -> MiniKanren.goal
      end

    val stepo : ProgramState.ti -> ProgramState.ti -> MiniKanren.goal

    val evalo :
      po:(ProgramState.ti -> MiniKanren.goal) ->
      ProgramState.ti -> ProgramState.ti -> MiniKanren.goal
  end

module ConcurrentInterpreter(Memory : MemoryModel) :
  sig
    module State :
      sig
        include Utils.Logic

        val init : Lang.RegStorage.ti -> Memory.ti -> ti

        val memo  : ?err:Lang.Error.ti -> ti -> Memory.ti -> MiniKanren.goal
        val regso : ?err:Lang.Error.ti -> ti -> Lang.ThreadID.ti -> Lang.Regs.ti -> MiniKanren.goal
        val regstorageo : ?err:Lang.Error.ti -> ti -> Lang.RegStorage.ti -> MiniKanren.goal

        val safeo : ti -> MiniKanren.goal

        val erroro :
         ?sg:(Lang.Error.ti -> MiniKanren.goal) ->
         ?fg:MiniKanren.goal ->
         ti -> MiniKanren.goal

        val dataraceo : ti -> MiniKanren.goal
      end

    module ProgramState :
      sig
        include Utils.Logic

        val make : Lang.CProg.ti -> State.ti -> ti

        val stateo : ti -> State.ti -> MiniKanren.goal

        val terminatedo : ?tid:Lang.ThreadID.ti -> ti -> MiniKanren.goal
      end

    val stepo : ?tid:Lang.ThreadID.ti -> ProgramState.ti -> ProgramState.ti -> MiniKanren.goal

    val evalo :
      tactic:Tactic.t ->
      po:(ProgramState.ti -> MiniKanren.goal) ->
      ProgramState.ti -> ProgramState.ti -> MiniKanren.goal

  end

module SequentialConsistent :
  sig
    include MemoryModel
  end

module ReleaseAcquire :
  sig
    include MemoryModel
  end
