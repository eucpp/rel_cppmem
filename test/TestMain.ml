open OUnit2

open MiniKanren

open Lang
open Lang.Stmt
open Lang.Expr
open Lang.Loc
open Lang.Reg
open Lang.Value
(* open MemoryModel *)
open Utils

let tests = Test.(
  make_testsuite ~name:"relcppmem" ~tests: [
    ProgTest.tests;
    LitmusTest.tests;
    (* SynthTest.tests; *)
  ]
)

let () =
  Test.ounit_run tests

  (* Test.simple_run tests;
  MiniKanren.report_counters () *)
