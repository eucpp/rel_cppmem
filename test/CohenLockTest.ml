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

let prog_CohenLock = <:cppmem_par<
  spw {{{
      r1 := choice(1, 2);
      x_rel := r1;
      repeat r2 := y_acq until (r2);
      if (r1 = r2) then
        (* start of critical section *)
        r3 := d_na;
        d_na := (r3 + 1)
        (* end of critical section *)
      else
        skip
      fi
  |||
    r1 := choice(1, 2);
    y_rel := r1;
    repeat r2 := x_acq until (r2);
    if (r1 != r2) then
      (* start of critical section *)
      r3 := d_na;
      d_na := (r3 + 1)
      (* end of critical section *)
    else
      skip
    fi
  }}}
>>

let thrdn = 2
let locs = ["x"; "y"; "d"]
let regs = ["r1"; "r2"; "r3"]

let test_SC =
  let module Memory = Operational.SequentialConsistent in
  let module Interpreter = ConcurrentInterpreter(Memory) in
  let module Trace = Utils.Trace(Interpreter.ProgramState) in

  let name = "SC" in

  let mem = Memory.alloc ~thrdn locs in
  let regs = RegStorage.init thrdn @@ Regs.alloc regs in
  let initstate = Interpreter.(ProgramState.make prog_CohenLock @@ State.init regs mem) in
  let asserto t =
    fresh (s m)
      (Interpreter.ProgramState.terminatedo t)
      (Interpreter.ProgramState.stateo t s)
      (conde
        [ (Interpreter.State.memo s m) &&& (Memory.checko m [("d", 2)])
        ; (Interpreter.State.erroro s)
        ]
      )
  in
  let evalo = Interpreter.evalo ~tactic:Tactic.Interleaving ~po:asserto in

  let pprint = Trace.trace in

  let test () =
    let stream = Query.eval evalo initstate in
    if Stream.is_empty stream then
      Test.Ok
    else begin
      Format.printf "Cohen Lock test (%s) fails!@;" name;
      let cexs = Stream.take stream in
      Format.printf "List of counterexamples:@;";
      List.iter (fun cex -> Format.printf "%a@;" pprint cex) cexs;
      Test.Fail ""
    end
  in

  Test.make_testcase ~name ~test


let test_RA =
  let module Memory = Operational.ReleaseAcquire in
  let module Interpreter = ConcurrentInterpreter(Memory) in
  let module Trace = Utils.Trace(Interpreter.ProgramState) in

  let name = "RA" in

  let mem = Memory.alloc ~thrdn locs in
  let regs = RegStorage.init thrdn @@ Regs.alloc regs in
  let initstate = Interpreter.(ProgramState.make prog_CohenLock @@ State.init regs mem) in
  let asserto t =
    fresh (s m)
      (Interpreter.ProgramState.terminatedo t)
      (Interpreter.ProgramState.stateo t s)
      (conde
        [ (Interpreter.State.memo s m) &&& (Memory.checko m [("d", 2)])
        ; (Interpreter.State.erroro s)
        ]
      )
  in
  let evalo = Interpreter.evalo ~tactic:Tactic.Interleaving in

  let pprint = Trace.trace in

  let test () =
    let stream = Query.eval (evalo ~po:asserto) initstate in
    if Stream.is_empty stream then
      Test.Ok
    else begin
      Format.printf "Cohen Lock test (%s) fails!@;" name;
      let cexs = Stream.take stream in
      Format.printf "List of counterexamples:@;";
      List.iter (fun cex -> Format.printf "%a@;" pprint cex) cexs;
      Test.Fail ""
    end
  in

  Test.make_testcase ~name ~test

let tests = Test.make_testsuite ~name:"CohenLock" ~tests:
  [
    test_SC;
    test_RA;
  ]
