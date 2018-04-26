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

type testkind = Safe | Unsafe | Synth

type test_desc =
  { name      : string
  ; prog      : Lang.Prog.ti list
  ; regs      : string list
  ; locs      : string list option
  ; mem       : (string * int) list option
  ; kind      : testkind
  ; prop      : Lang.Prop.ti
  ; n         : int option
  }

let make_test_desc ?n ?locs ?mem ~regs ~name ~kind ~prop prog =
  { name; prog; regs; locs; mem; kind; prop; n }

module OperationalTest(Memory : Operational.MemoryModel) =
  struct
    module Interpreter = Operational.Interpreter(Memory)
    module State = Interpreter.State

    let make_istate ~regs ?locs ?mem prog =
      match mem with
      | Some mem -> Interpreter.State.init_istate ~regs ~mem prog
      | None     ->
        match locs with
        | Some locs -> Interpreter.State.alloc_istate ~regs ~locs prog
        | None      -> failwith "Cannot make initial state"

    let test_unsafe ~name ~prop ~prog ~regs ~locs ~mem =
      let module Trace = Utils.Trace(State) in
      let istate = make_istate ~regs ?locs ?mem prog in
      let stream = Interpreter.eval ~prop:(Lang.Prop.neg prop) istate in
      if not @@ Stream.is_empty stream then
        Ok
      else
        let outs = Interpreter.eval istate in
        Format.printf "Test %s::%s fails!@;" Memory.name name;
        Format.printf "List of outputs:@;";
        Stream.iter (fun out -> Format.printf "%a@;" Trace.trace out) outs;
        Fail ""

    let test_safe ~name ~prop ~prog ~regs ~locs ~mem =
      let module Trace = Utils.Trace(State) in
      let istate = make_istate ~regs ?locs ?mem prog in
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

    let test_synth ?n ~name ~prop ~prog ~regs ~locs ~mem =
      let module Trace = Utils.Trace(Lang.CProg) in
      let istate = make_istate ~regs ?locs ?mem prog in
      let stream = run q
        (fun p ->
            fresh (s s' tm)
              (State.instmo istate s)
              (State.thrdmgro s tm)
              (Lang.ThreadManager.cprogo tm p)
              (Interpreter.reachableo s s')
              (State.terminatedo s')
              (State.sato prop s')
            ?~(fresh (s'')
                  (Interpreter.reachableo s s'')
                  (State.terminatedo s'')
                ?~(State.sato prop s'')
              )
        )
        (fun qs -> qs)
      in
      if Stream.is_empty stream then begin
        Format.printf "Test %s fails: !@;" name;
        Format.printf "No programs were synthesized@;";
        Fail ""
        end
      else begin
        let progs = Stream.take ?n stream in
        Format.printf "Test %s succeeded!@;" name;
        Format.printf "List of synthesized programs:@\n";
        let cnt = ref 0 in
        List.iter (fun p ->
          cnt := !cnt + 1;
          Format.printf "Program #%d:@\n%a@\n" !cnt Trace.trace p
        ) progs;
        Ok
      end

    let make_testcase { name; prog; prop; regs; locs; mem; kind; n } =
      let full_name = Printf.sprintf "%s::%s" Memory.name name in
      let test () =
        match kind with
        | Unsafe  -> test_unsafe ~name:full_name ~prop ~prog ~regs ~locs ~mem
        | Safe    -> test_safe   ~name:full_name ~prop ~prog ~regs ~locs ~mem
        | Synth   -> test_synth  ~name:full_name ~prop ~prog ~regs ~locs ~mem ?n
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
