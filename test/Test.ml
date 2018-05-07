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

type testlen = test_length

type testctx = test_ctxt

type result = Ok | Fail of string

type testfun = testctx -> result

let get_fullname : testctx -> string = fun test_ctx -> OUnitTest.string_of_path test_ctx.path

type test =
  | TestCase  of string * testlen * testfun
  | TestSuite of string * test list

let make_testcase ?(length=OUnitTest.Short) ~name test = TestCase (name, length, test)

let make_testsuite ~name tests = TestSuite (name, tests)

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
  ; len       : testlen option
  }

let make_test_desc ?n ?locs ?mem ?len ~regs ~name ~kind ~prop prog =
  { name; prog; regs; locs; mem; kind; prop; n; len }

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

    let test_unsafe ~name ~prop ~prog ~regs ~locs ~mem ~n test_ctx =
      let module Trace = Utils.Trace(State) in
      let istate = make_istate ~regs ?locs ?mem prog in
      (* [test_unsafe] takes proposition on the final (terminal) state of program,
       *   thus we add `not terminal` disjunct to invariant
       *)
      let prop = Lang.(Prop.((neg @@ terminated ()) || prop)) in
      if not @@ Interpreter.invariant ~prop istate then
        Ok
      else
        let outs = Stream.take ?n @@ Interpreter.eval istate in
        Format.printf "Test %s fails!@;" @@ get_fullname test_ctx;
        Format.printf "List of outputs (%d):@;" @@ List.length outs;
        List.iter (fun out -> Format.printf "%a@;" Trace.trace out) outs;
        Fail ""

    let test_safe ?n ~name ~prop ~prog ~regs ~locs ~mem test_ctx =
      let module Trace = Utils.Trace(State) in
      let istate = make_istate ~regs ?locs ?mem prog in
      (* [test_safe] takes proposition on the final (terminal) state of program,
       *   thus we add `not terminal` disjunct to invariant
       *)
      let prop = Lang.(Prop.((neg @@ terminated ()) || prop)) in
      if Interpreter.invariant ~prop istate then
        Ok
      else begin
        let cexs = Stream.take ?n @@ Interpreter.reachable ~prop:(Lang.Prop.neg prop) istate in
        Format.printf "Test %s fails!@;" @@ get_fullname test_ctx;
        Format.printf "List of counterexamples (%d):@;" @@ List.length cexs;
        List.iter (fun cex -> Format.printf "%a@;" Trace.trace cex) cexs;
        Fail ""
      end

    let test_synth ?n ~name ~prop ~prog ~regs ~locs ~mem test_ctx =
      let module Trace = Utils.Trace(Lang.CProg) in
      let istate = make_istate ~regs ?locs ?mem prog in
      let stream = run q
        (fun p ->
            fresh (s s' tm)
              (State.instmo istate s)
              (State.thrdmgro s tm)
              (Lang.ThreadManager.cprogo tm p)
              (Interpreter.evalo ~prop s s')
              (Interpreter.invarianto ~prop:Lang.(Prop.((neg @@ terminated ()) || prop)) s)
        )
        (fun qs -> qs)
      in
      let progs = Stream.take ?n stream in
      if List.length progs = 0 then begin
        Format.printf "Test %s fails!@;" @@ get_fullname test_ctx;
        Format.printf "No programs were synthesized@;";
        Fail ""
        end
      else begin
        Format.printf "Test %s succeeded!@;" @@ get_fullname test_ctx;
        Format.printf "List of synthesized programs (%d):@\n" @@ List.length progs;
        let cnt = ref 0 in
        List.iter (fun p ->
          cnt := !cnt + 1;
          Format.printf "Program #%d:@\n%a@\n" !cnt Trace.trace p
        ) progs;
        Ok
      end

    let make_testcase { name; prog; prop; regs; locs; mem; kind; n; len } =
      let test =
        match kind with
        | Unsafe  -> test_unsafe ~name ~prop ~prog ~regs ~locs ~mem ?n
        | Safe    -> test_safe   ~name ~prop ~prog ~regs ~locs ~mem ?n
        | Synth   -> test_synth  ~name ~prop ~prog ~regs ~locs ~mem ?n
      in
      make_testcase ?length:len ~name test

    let make_testsuite tests =
      make_testsuite ~name:Memory.name (List.map make_testcase tests)
  end

let make_operational_testsuite ~model tests =
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

(* let rec simple_run = function
  | TestCase  (name, len, test)  -> let _ = test () in ()
  | TestSuite (name, tests)      -> List.iter simple_run tests *)

let rec ounit_test = function
  | TestCase (name, length, test)  -> name >: (
      test_case ~length @@ fun test_ctx ->
        match test test_ctx with
        | Ok      -> ()
        | Fail s  -> assert_failure s
    )
  | TestSuite (name, tests) -> name >::: (List.map ounit_test tests)

let ounit_run t = run_test_tt_main @@ ounit_test t
