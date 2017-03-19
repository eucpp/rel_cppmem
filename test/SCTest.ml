open OUnit2
open MiniKanren
open Memory

module T = Lang.Term
module Sem = Semantics.Make(Lang.Term)(Lang.Context)(MemState)

let sem = Sem.make @@ List.append Rules.Basic.all Rules.SeqCons.all

let test_prog = TestUtils.test_prog sem

let prog_LB = "
    x_sc := 0;
    y_sc := 0;
    spw {{{
        r1 := x_sc;
        y_sc := 1;
        ret r1
    |||
        r2 := y_sc;
        x_sc := 1;
        ret r2
    }}}"

(* let prog_LB = " *)
(*     x_sc := 0; *)
(*     y_sc := 0; *)
(*     spw {{{ *)
(*         ret x_sc *)
(*     ||| *)
(*         ret y_sc *)
(*     }}}" *)

let test_LB =
  "LB">: OUnitTest.TestCase (OUnitTest.Long, test_prog prog_LB ["(0, 0)"; "(1, 0)"; "(0, 1)"])

let tests =
  "SC">::: [test_LB]
