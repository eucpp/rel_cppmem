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
    LitmusTest.tests;
    (* SynthTest.tests; *)
  ]
)

(* let test_prog = <:cppmem<
  assert (
    (r1 = 0 && r2 = 0) ||
    (r1 = 0 && r2 = 1) ||
    (r1 = 1 && r2 = 1)
  )
>> *)

(* let _ =
  let module Trace = Utils.Trace(ReleaseAcquire.State) in
  let regs = ["r1"; "r2"; "r3"; "r4"] in
  let mem = [("x", 0); ("f", 0)] in
  let s =
    Query.exec
      ReleaseAcquire.intrpo
      (* LitmusTest.prog_CoRR *)
      test_prog
      (ReleaseAcquire.State.mem @@ ReleaseAcquire.Memory.init ~regs ~mem)
  in
  Stream.iter (Trace.trace Format.std_formatter) s;
  report_counters () *)


let () =
  Test.ounit_run tests

  (* Test.simple_run tests;
  MiniKanren.report_counters () *)
