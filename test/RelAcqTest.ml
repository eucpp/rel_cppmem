open OUnit2
open MiniKanren
open Memory

let sem = Semantics.make @@ List.append Rules.Basic.all Rules.RelAcq.all

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

let prog_MP = "
    x_rel := 0;
    f_rel := 0;
    spw {{{
        x_rel := 1;
        f_rel := 1;
        ret x_rel
    |||
        repeat f_acq end;
        ret x_rel
    }}}"

let tests =
  "relAcq">::: [
    "LB">:: test_prog prog_LB ["(0, 0)"; "(1, 0)"; "(0, 1)"];
    (* "MP">:: test_prog prog_MP ["(1, 1)";]; *)
  ]
