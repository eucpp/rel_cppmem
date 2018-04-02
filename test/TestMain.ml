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

open OUnit2

open MiniKanren

open Lang
open Lang.Stmt
open Lang.Expr
open Lang.Loc
open Lang.Reg
open Lang.Value
(* open MemoryModel *)
open Utils

let tests = Test.(
  make_testsuite ~name:"relcppmem" ~tests: [
    ProgTest.tests;
    LitmusTest.tests;
    (* DekkerLockTest.tests; *)
    (* CohenLockTest.tests; *)
    (* BarrierTest.tests; *)
    (* SynthTest.tests; *)
  ]
)

let () =
  (* Test.ounit_run tests *)

  Test.simple_run tests;
  MiniKanren.report_counters ()
