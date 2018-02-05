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

type result = Ok | Fail of string

type testcase = unit -> result

type test =
  | TestCase  of string * testcase
  | TestSuite of string * test list

let make_testcase ~name ~test = TestCase (name, test)

let make_testsuite ~name ~tests = TestSuite (name, tests)

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
