open OUnit2

let tests = 
  "rel_cppmem">::: [LangTest.tests; 
                    MemoryTest.tests;
                    RulesTest.tests;
                    ParserTest.tests; 
                    PrettyPrinterTest.tests;
                    RelAcqTest.tests]

let () =
  run_test_tt_main tests
