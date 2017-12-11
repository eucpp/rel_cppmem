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
