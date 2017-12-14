open OUnit2

open MiniKanren

open Lang
open Lang.Term
open Lang.Expr
open Lang.Loc
open Lang.Register
open Lang.Value
open MemoryModel
open Utils

let tests = Test.(
  make_testsuite ~name:"relcppmem" ~tests: [
    (* LitmusTest.tests; *)
    SynthTest.tests;
  ]
)


let () =
  Test.ounit_run tests

  (* Test.simple_run tests;
  MiniKanren.report_counters () *)
