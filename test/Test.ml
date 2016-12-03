open OUnit2

let tests = 
  "rel_cppmem">::: [LangTest.tests; MemoryTest.tests]

let () =
  run_test_tt_main tests
