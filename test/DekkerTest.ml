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

open MiniKanren
(* open MiniKanren.Std *)

open Lang
open Lang.Expr
open Lang.Stmt
open Lang.Loc
open Lang.Reg
open Lang.Value

let prog_Dekker = <:cppmem_par<
  spw {{{
      x_sc := 1;
      r1 := y_sc;
      while (r1) do
        r2 := turn_sc;
        if (r2 != 0) then
          x_sc := 0;
          repeat r2 := turn_sc until (r2 = 0);
          x_sc := 1
        else
          skip
        fi;
        r1 := y_sc
      od;
      (* start of critical section *)
      r3 := v_sc;
      v_sc := (r3 + 1);
      (* end of critical section *)
      turn_sc := 1;
      x_sc := 0
  |||
      y_sc := 1;
      r1 := x_sc;
      while (r1) do
        r2 := turn_sc;
        if (r2 != 1) then
          y_sc := 0;
          repeat r2 := turn_sc until (r2 = 1);
          y_sc := 1
        else
          skip
        fi;
        r1 := x_sc
      od;
      (* start of critical section *)
      r3 := v_sc;
      v_sc := (r3 + 1);
      (* end of critical section *)
      turn_sc := 0;
      y_sc := 0
  }}}
>>

(* let () =
  let module Memory = Operational.SequentialConsistent in
  let module Interpreter = ConcurrentInterpreter(Memory) in
  let module Trace = Utils.Trace(Interpreter.State) in

  let thrdn = 2 in
  let locs = ["x"; "y"; "turn"; "v"] in
  let regs = ["r1"; "r2"; "r3"] in

  let mem = Memory.alloc ~thrdn locs in
  let regs = RegStorage.init thrdn @@ Regs.alloc regs in
  let initstate = Interpreter.State.init regs mem in

  let stream = Query.exec (Interpreter.interpo Tactic.Interleaving) prog_Dekker initstate in

  Stream.iter (fun s ->
    Format.printf "%a" Trace.trace s
  ) stream *)

let () =
  let module Memory = Operational.ReleaseAcquire in
  let module Interpreter = ConcurrentInterpreter(Memory) in
  let module Trace = Utils.Trace(Interpreter.State) in

  let thrdn = 2 in
  let locs = ["x"; "y"; "turn"; "v"] in
  let regs = ["r1"; "r2"; "r3"] in

  let mem = Memory.alloc ~thrdn locs in
  let regs = RegStorage.init thrdn @@ Regs.alloc regs in
  let initstate = Interpreter.State.init regs mem in

  let stream = Query.exec (Interpreter.interpo Tactic.Interleaving) prog_Dekker initstate in

  Stream.iter (fun s ->
    Format.printf "%a" Trace.trace s
  ) stream

let tests = Test.make_testsuite ~name:"Dekker" ~tests: []