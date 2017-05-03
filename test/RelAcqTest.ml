open OUnit2
open MiniKanren
open Memory
open TestUtils

let prog_LB = "
    x_rlx := 0;
    y_rlx := 0;
    spw {{{
        r1 := x_acq;
        y_rel := 1;
        ret r1
    |||
        r2 := y_acq;
        x_rel := 1;
        ret r2
    }}}"

let test_LB = test_prog prog_LB ["(0, 0)"; "(1, 0)"; "(0, 1)"]

let prog_MP = "
    x_rlx := 0;
    f_rlx := 0;
    spw {{{
        x_rlx := 1;
        f_rel := 1;
        ret 1
    |||
        repeat f_acq end;
        r2 := x_rlx;
        ret r2
    }}}"

let test_MP = test_prog prog_MP ["(1, 1)"]

let prog_MP_rlx_1 = "
    x_rlx := 0;
    f_rlx := 0;
    spw {{{
        x_rlx := 1;
        f_rlx := 1;
        ret 1
    |||
        repeat f_acq end;
        r2 := x_rlx;
        ret r2
    }}}"

let test_MP_rlx_1 = test_prog prog_MP_rlx_1 ["(1, 0)"; "(1, 1)"]

let prog_MP_rlx_2 = "
    x_rlx := 0;
    f_rlx := 0;
    spw {{{
        x_rlx := 1;
        f_rel := 1;
        ret 1
    |||
        repeat f_rlx end;
        r2 := x_rlx;
        ret r2
    }}}"

let test_MP_rlx_2 = test_prog prog_MP_rlx_2 ["(1, 0)"; "(1, 1)"]

let prog_MP_rel_seq = "
  x_rlx := 0;
  f_rlx := 0;
  spw {{{
      x_rlx := 1;
      f_rel := 1;
      f_rlx := 2;
      ret 1
  |||
      repeat f_acq = 2 end;
      r2 := x_rlx;
      ret r2
}}}"

let test_MP_rel_seq = test_prog prog_MP_rel_seq ["(1, 1)"]

let tests =
  "relAcq">::: [
    "LB">:: test_LB;
    "MP">:: test_MP;
    "MP_rlx_1">:: test_MP_rlx_1;
    "MP_rlx_2">:: test_MP_rlx_2;
    "MP_rel_seq">:: test_MP_rel_seq;
  ]
