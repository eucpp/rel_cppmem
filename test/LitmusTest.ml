open OUnit2
open MiniKanren
open TestUtils

open Lang
open Lang.Term

let prog_rel_acq = <:cppmem<
    x_rlx := 0;
    f_rlx := 0;
    spw {{{
        x_rlx := 1;
        f_rel := 1
    |||
        r1 := f_acq;
        r2 := x_rlx;
        (r1, r2)
    }}}
>>

let test_rel_acq = test_prog prog_rel_acq ["(0, 0)"; "(0, 1)"; "(1, 1)";]

let prog_SB = <:cppmem<
  x_rlx := 0;
  y_rlx := 0;
  spw {{{
      x_rel := 1;
      r1 := y_acq;
      ret r1
  |||
      y_rel := 1;
      r2 := x_acq;
      ret r2
  }}}
>>

let test_SB = test_prog prog_SB ["(0, 0)"; "(1, 0)"; "(0, 1)"; "(1, 1)"]

let prog_LB_rel_acq = <:cppmem<
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
    }}}
>>

let test_LB_rel_acq = test_prog prog_LB_rel_acq ["(0, 0)"; "(1, 0)"; "(0, 1)"]

let prog_LB_rel_acq_rlx = <:cppmem<
    x_rlx := 0;
    y_rlx := 0;
    spw {{{
        r1 := x_acq;
        y_rlx := 1;
        ret r1
    |||
        r2 := y_rlx;
        x_rel := 1;
        ret r2
    }}}
>>

let test_LB_rel_acq_rlx = test_prog prog_LB_rel_acq_rlx ["(0, 0)"; "(1, 0)"; "(0, 1)"]

let prog_MP = <:cppmem<
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
    }}}
>>

let test_MP = test_prog prog_MP ["(1, 1)"]

let prog_MP_rlx_1 = <:cppmem<
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
    }}}
>>

let test_MP_rlx_1 = test_prog prog_MP_rlx_1 ["(1, 0)"; "(1, 1)"]

let prog_MP_rlx_2 = <:cppmem<
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
    }}}
>>

let test_MP_rlx_2 = test_prog prog_MP_rlx_2 ["(1, 0)"; "(1, 1)";]

let prog_MP_rel_seq = <:cppmem<
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
  }}}
>>

let test_MP_rel_seq = test_prog prog_MP_rel_seq ["(1, 1)"]

let prog_CoRR_rlx = <:cppmem<
  x_rlx := 0;
  spw {{{
    spw {{{
      x_rlx := 1
    |||
      x_rlx := 2
    }}}
  |||
    spw {{{
      r1 := x_rlx;
      r2 := x_rlx;
      ret (r1, r2)
    |||
      r3 := x_rlx;
      r4 := x_rlx;
      ret (r3, r4)
    }}}
  }}}
>>

(* let test_CoRR_rlx = test_prog ~negative:true prog_CoRR_rlx ["((1, 2), (2, 1))"; "((2, 1), (1, 2))"] *)

let tests =
  "Litmus">::: [
    "rel_acq">:: test_rel_acq;
    "SB">:: test_SB;
    "LB_rel_acq">:: test_LB_rel_acq;
    "LB_rel_acq_rlx">:: test_LB_rel_acq_rlx;
    "MP">:: test_MP;
    "MP_rlx_1">:: test_MP_rlx_1;
    "MP_rlx_2">:: test_MP_rlx_2;
    "MP_rel_seq">:: test_MP_rel_seq;
    (* "CoRR_rlx">: OUnitTest.TestCase (OUnitTest.Short, test_CoRR_rlx); *)
  ]
