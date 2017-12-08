open OUnit2

type result = Ok | Fail of string

type testcase =
  { name : string
  ; test : unit -> result
  }

let ounit_test {name; test} =
  name >:: fun test_ctx -> match test () with Ok -> () | Fail s -> assert_failure s

let run {name; test} =
  let res = test () in ()
