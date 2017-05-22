open OUnit2
open MiniKanren
open TestUtils

open Lang
open Lang.Term
open Memory

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

let test_rel_acq step = test_prog step prog_rel_acq ["(0, 0)"; "(0, 1)"; "(1, 1)";]

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

let test_SB step = test_prog step prog_SB ["(0, 0)"; "(1, 0)"; "(0, 1)"; "(1, 1)"]

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

let test_LB_rel_acq step = test_prog step prog_LB_rel_acq ["(0, 0)"; "(1, 0)"; "(0, 1)"]

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

let test_LB_rel_acq_rlx step = test_prog step prog_LB_rel_acq_rlx ["(0, 0)"; "(1, 0)"; "(0, 1)"]

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

let test_MP step = test_prog step prog_MP ["(1, 1)"]

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

let test_MP_rlx_1 step = test_prog step prog_MP_rlx_1 ["(1, 0)"; "(1, 1)"]

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

let test_MP_rlx_2 step = test_prog step prog_MP_rlx_2 ["(1, 0)"; "(1, 1)";]

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

let test_MP_rel_seq step = test_prog step prog_MP_rel_seq ["(1, 1)"]

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

(* let test_CoRR_rlx step = test_prog step ~negative:true prog_CoRR_rlx ["((1, 2), (2, 1))"; "((2, 1), (1, 2))"] *)

(* let prog_LB = <:cppmem<
  spw {{{
    r1 := x_rlx;
    y_rlx := 1;
    ret r1
  |||
    r2 := y_rlx;
    x_rlx := r2
  }}}
>> *)

let prog_LB = <:cppmem<
  spw {{{
    x_rlx := 1;
    ret 2
  |||
    y_rlx := 1
  }}}
>>

let test_LB step = test_prog step prog_LB ["0"; "1"]

let rlx_rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.Rlx.all
let rlx_relAcq_rules = rlx_rules @ Rules.RelAcq.all

let promising_rules = rlx_rules @ [Rules.Promise.fulfill]

let relAcqStep = make_reduction_relation rlx_relAcq_rules
let promisingStep = make_certified_relation promising_rules

let tests =
  "Litmus">::: [
    "LB">: OUnitTest.TestCase (OUnitTest.Huge, test_LB promisingStep);

    "rel_acq">:: test_rel_acq relAcqStep;
    "SB">:: test_SB relAcqStep;
    "LB_rel_acq">:: test_LB_rel_acq relAcqStep;
    "LB_rel_acq_rlx">:: test_LB_rel_acq_rlx relAcqStep;
    "MP">:: test_MP relAcqStep;
    "MP_rlx_1">:: test_MP_rlx_1 relAcqStep;
    "MP_rlx_2">:: test_MP_rlx_2 relAcqStep;
    "MP_rel_seq">:: test_MP_rel_seq relAcqStep;
    (* "CoRR_rlx">: OUnitTest.TestCase (OUnitTest.Short, test_CoRR_rlx relAcqStep); *)
  ]
