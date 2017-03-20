open OUnit2
open MiniKanren
open Memory

module T = Lang.Term
module Sem = Semantics.Make(Lang.Term)(Lang.Context)(MemState)

let sem = Sem.make @@ List.append Rules.Basic.all Rules.RelAcq.all

let test_prog = TestUtils.test_prog sem

let prog_LB = "
    x_rel := 0;
    y_rel := 0;
    spw {{{
        r1 := x_acq;
        y_rel := 1;
        ret r1
    |||
        r2 := y_acq;
        x_rel := 1;
        ret r2
    }}}"

let test_LB =
  "LB">: OUnitTest.TestCase (OUnitTest.Long, test_prog prog_LB ["(0, 0)"; "(1, 0)"; "(0, 1)"])

let tests =
  "relAcq">::: [test_LB]
