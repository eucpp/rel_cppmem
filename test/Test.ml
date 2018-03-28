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

type result = Ok | Fail of string

type testcase = unit -> result

type test =
  | TestCase  of string * testcase
  | TestSuite of string * test list

let make_testcase ~name ~test = TestCase (name, test)

let make_testsuite ~name ~tests = TestSuite (name, tests)

type memory_model = SeqCst | TSO | RelAcq

type status = Violates | Fulfills

type test_desc =
  { name      : string
  ; prog      : Lang.Prog.ti list
  ; regs      : string list
  ; locs      : string list
  ; stat      : status
  ; prop      : Lang.Prop.ti
  }

let make_test_desc ~name ~prog ~regs ~locs ~stat ~prop =
  { name; prog; regs; locs; stat; prop }

module OperationalTest(Memory : Operational.MemoryModel.T) =
  struct
    module Interpreter = Operational.Interpreter(Memory)
    module Trace = Utils.Trace(Interpreter.State)

    let test_violates ~name ~prop ~prog ~regs ~locs =
      let istate = Interpreter.State.alloc_istate ~regs ~locs prog in
      let stream = Interpreter.eval ~prop:(Lang.Prop.neg prop) istate in
      if not @@ Stream.is_empty stream then
        Ok
      else
        let outs = Interpreter.eval istate in
        Format.printf "Test %s::%s fails!@;" Memory.name name;
        Format.printf "List of outputs:@;";
        Stream.iter (fun out -> Format.printf "%a@;" Trace.trace out) outs;
        Fail ""

    let test_fulfills ~name ~prop ~prog ~regs ~locs =
      let istate = Interpreter.State.alloc_istate ~regs ~locs prog in
      (* check that test is runnable *)
      (* let stream = Interpreter.eval istate in
      if Stream.is_empty stream then begin
        Format.printf "Litmus test %s is not runnable!@;" name;
        Test.Fail ""
        end
      else *)
        let stream = Interpreter.eval ~prop:(Lang.Prop.neg prop) istate in
        if Stream.is_empty stream then
          Ok
        else begin
          let cexs = Stream.take stream in
          Format.printf "Test %s fails!@;" name;
          Format.printf "List of counterexamples:@;";
          List.iter (fun cex -> Format.printf "%a@;" Trace.trace cex) cexs;
          Fail ""
        end

    let make_testcase {name; prog; prop; regs; locs; stat} =
      let full_name = Printf.sprintf "%s::%s" Memory.name name in
      let test () =
        match stat with
        | Violates -> test_violates ~name:full_name ~prop ~prog ~regs ~locs
        | Fulfills -> test_fulfills ~name:full_name ~prop ~prog ~regs ~locs
      in
      make_testcase ~name ~test

    let make_testsuite tests =
      make_testsuite ~name:Memory.name ~tests:(List.map make_testcase tests)
  end

let make_operational_testsuite ~model ~tests =
  match model with
  | SeqCst ->
    let module Test = OperationalTest(Operational.SeqCst) in
    Test.make_testsuite tests
  | TSO ->
    let module Test = OperationalTest(Operational.TSO) in
    Test.make_testsuite tests
  | RelAcq ->
    let module Test = OperationalTest(Operational.RelAcq) in
    Test.make_testsuite tests

let rec simple_run = function
  | TestCase  (name, test)  -> let _ = test () in ()
  | TestSuite (name, tests) -> List.iter simple_run tests

let rec ounit_test = function
  | TestCase (name, test)  -> name >:: fun test_ctx ->
    begin match test () with
    | Ok -> ()
    | Fail s -> assert_failure s
    end
  | TestSuite (name, tests) -> name >::: (List.map ounit_test tests)

let ounit_run t = run_test_tt_main @@ ounit_test t
