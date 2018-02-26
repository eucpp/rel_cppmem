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

let prog_Barrier = <:cppmem_par<
  spw {{{
    x_na := 1;
    (* barrier start *)
    repeat
      r1 := cnt_rlx;
      r2 := CAS(relAcq, rlx, cnt, r1, (r1 - 1))
    until (r1 = r2);
    if (r2 = 1) then
      g_rel := 1
    else
      repeat r1 := g_acq until r1
    fi;
    (* barrier end *)
    r3 := y_na
  |||
    y_na := 1;
    (* barrier start *)
    repeat
      r1 := cnt_rlx;
      r2 := CAS(relAcq, rlx, cnt, r1, (r1 - 1))
    until (r1 = r2);
    if (r2 = 1) then
      g_rel := 1
    else
      repeat r1 := g_acq until r1
    fi;
    (* barrier end *)
    r3 := x_na
  }}}
>>

let thrdn = 2
let locs = [("x", 0); ("y", 0); ("g", 0); ("cnt", 2)]
let regs = ["r1"; "r2"; "r3"]

let test_SC =
  let module Memory = Operational.SequentialConsistent in
  let module Interpreter = Operational.ConcurrentInterpreter(Memory) in
  let module Trace = Utils.Trace(Interpreter.ProgramState) in

  let name = "SC" in

  let mem = Memory.init ~thrdn locs in
  let regs = RegStorage.init thrdn @@ Regs.alloc regs in
  let initstate = Interpreter.(ProgramState.make prog_Barrier @@ State.init regs mem) in
  let asserto t =
    fresh (s m)
      (Interpreter.ProgramState.terminatedo t)
      (Interpreter.ProgramState.stateo t s)
      (conde
        [ (Interpreter.State.erroro s)
        ; fresh (rs1 rs2 v1 v2)
            (Interpreter.State.regso s (ThreadID.tid 1) rs1)
            (Interpreter.State.regso s (ThreadID.tid 1) rs2)
            (Regs.reado rs1 (Reg.reg "r3") v1)
            (Regs.reado rs2 (Reg.reg "r3") v2)
            ((v1 =/= (Value.integer 1)) ||| (v2 =/= (Value.integer 1)))
        ]
      )
  in
  let evalo = Interpreter.evalo ~tactic:Operational.Tactic.Interleaving in

  let pprint = Trace.trace in

  let test () =
    let stream = Query.eval (evalo ~po:asserto) initstate in
    if Stream.is_empty stream then
      Test.Ok
    else begin
      Format.printf "Barrier test (%s) fails!@;" name;
      let cexs = Stream.take stream in
      Format.printf "List of counterexamples:@;";
      List.iter (fun cex -> Format.printf "%a@;" pprint cex) cexs;
      Test.Fail ""
    end
  in

  Test.make_testcase ~name ~test

let test_RA =
  let module Memory = Operational.ReleaseAcquire in
  let module Interpreter = Operational.ConcurrentInterpreter(Memory) in
  let module Trace = Utils.Trace(Interpreter.ProgramState) in

  let name = "RA" in

  let mem = Memory.init ~thrdn locs in
  let regs = RegStorage.init thrdn @@ Regs.alloc regs in
  let initstate = Interpreter.(ProgramState.make prog_Barrier @@ State.init regs mem) in
  let asserto t =
    fresh (s m)
      (Interpreter.ProgramState.terminatedo t)
      (Interpreter.ProgramState.stateo t s)
      (conde
        [ (Interpreter.State.erroro s)
        ; fresh (rs1 rs2 v1 v2)
            (Interpreter.State.regso s (ThreadID.tid 1) rs1)
            (Interpreter.State.regso s (ThreadID.tid 1) rs2)
            (Regs.reado rs1 (Reg.reg "r3") v1)
            (Regs.reado rs2 (Reg.reg "r3") v2)
            ((v1 =/= (Value.integer 1)) ||| (v2 =/= (Value.integer 1)))
        ]
      )
  in
  let evalo = Interpreter.evalo ~tactic:Operational.Tactic.Interleaving in

  let pprint = Trace.trace in

  let test () =
    let stream = Query.eval (evalo ~po:asserto) initstate in
    if Stream.is_empty stream then
      Test.Ok
    else begin
      Format.printf "Barrier test (%s) fails!@;" name;
      let cexs = Stream.take stream in
      Format.printf "List of counterexamples:@;";
      List.iter (fun cex -> Format.printf "%a@;" pprint cex) cexs;
      Test.Fail ""
    end
    (* let stream = Query.eval (evalo ~po:Interpreter.ProgramState.terminatedo) initstate in
    let cexs = Stream.take stream in
    Format.printf "List of counterexamples:@;";
    List.iter (fun cex -> Format.printf "%a@;" pprint cex) cexs;
    Test.Ok *)
  in

  Test.make_testcase ~name ~test


let tests = Test.make_testsuite ~name:"Barrier" ~tests:
  [
    test_SC;
    test_RA;
  ]
