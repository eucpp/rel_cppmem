open OUnit2
open MiniKanren
open Memory
open TestUtils

(* let sem = Semantics.make @@ List.append Rules.Basic.all Rules.RelAcq.all *)

(* let test_prog ?(n:int) = TestUtils.test_prog ~n sem *)

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

let prog_MP = "
    x_rel := 0;
    f_rel := 0;
    spw {{{
        x_rel := 1;
        f_rel := 1;
        ret 1
    |||
        repeat f_acq end;
        r2 := x_acq;
        ret r2
    }}}"

(* let prog_MP_partial = "
  x_rel := 0;
  f_rel := 0;
  spw {{{
      ?q;
      f_rel := 1;
      ret 1
  |||
      repeat f_acq end;
      r2 := x_acq;
      ret r2
  }}}
" *)

let prog_MP_partial = "?q; ret r2"

(* let step = (module Semantics.OperationalStep : Semantics.Step) *)

let tests =
  "relAcq">::: [
    "LB">:: test_prog prog_LB ["(0, 0)"; "(1, 0)"; "(0, 1)"];
    "MP">:: test_prog ~n:100 prog_MP ["(1, 1)";];
    (* "MP_partial">:: test_prog_synthesis ~n:10 sem prog_MP ["f_rel := 1"]; *)
  ]
