open OUnit2
open Memory

let tests =
  "rel_cppmem">::: [
    LangTest.tests;
    VarListTest.tests;
    MemoryTest.tests;
    ParserTest.tests;
    PrettyPrinterTest.tests;
    RulesTest.tests;
    LitmusTest.tests;
    SynthesisTest.tests;
  ]

let () =
  run_test_tt_main tests
