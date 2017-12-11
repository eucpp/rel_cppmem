open OUnit2

let tests = Test.(
  make_testsuite ~name:"relcppmem" ~tests: [
    LitmusTest.tests;
    (* SynthTest.tests; *)
  ]
)

let () =
  Test.ounit_run tests

  (* Test.simple_run tests;
  MiniKanren.report_counters () *)
