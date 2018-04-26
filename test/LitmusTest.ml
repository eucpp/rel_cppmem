(* Copyright (c) 2016-2018
 * Evgenii Moiseenko and Anton Podkopaev
 * St.Petersburg State University, JetBrains Research
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

open MiniKanren
(* open MiniKanren.Std *)

open Lang
open Lang.Expr
open Lang.Stmt
open Lang.Loc
open Lang.Reg
open Lang.Value

(* ************************************************************************** *)
(* ******************* SequentialConsistent Tests *************************** *)
(* ************************************************************************** *)

let prog_SW = <:cppmem_par<
  spw {{{
      x_sc := 1;
      f_sc := 1
  |||
      r1 := f_sc;
      r2 := x_sc
  }}}
>>

let test_SW = Test.(make_test_desc
  ~name:"SW"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~kind:Safe
  ~prop:Prop.(
       ((2%"r1" = 0) && (2%"r2" = 0))
    || ((2%"r1" = 0) && (2%"r2" = 1))
    || ((2%"r1" = 1) && (2%"r2" = 1))
  )
  prog_SW
)

let prog_SB = <:cppmem_par<
  spw {{{
      x_sc := 1;
      r1 := y_sc
  |||
      y_sc := 1;
      r2 := x_sc
  }}}
>>

let test_SB = Test.(make_test_desc
  ~name:"SB"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~kind:Safe
  ~prop:Prop.(
    !((1%"r1" = 0) && (2%"r2" = 0))
  )
  prog_SB
)

let prog_LB = <:cppmem_par<
    spw {{{
        r1 := x_sc;
        y_sc := 1
    |||
        r2 := y_sc;
        x_sc := 1
    }}}
>>

let test_LB = Test.(make_test_desc
  ~name:"LB"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~kind:Safe
  ~prop:Prop.(
    !((1%"r1" = 1) && (2%"r2" = 1))
  )
  prog_LB
)

let prog_MP = <:cppmem_par<
    spw {{{
        x_sc := 1;
        f_sc := 1
    |||
        repeat r1 := f_sc until r1;
        r2 := x_sc
    }}}
>>

let test_MP = Test.(make_test_desc
  ~name:"MP"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~kind:Safe
  ~prop:Prop.(
    (2%"r2" = 1)
  )
  prog_MP
)

let prog_CoRR = <:cppmem_par<
  spw {{{
    x_sc := 1
  |||
    x_sc := 2
  |||
    r1 := x_sc;
    r2 := x_sc
  |||
    r3 := x_sc;
    r4 := x_sc
  }}}
>>

let test_CoRR = Test.(make_test_desc
  ~name:"CoRR"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";]
  ~kind:Safe
  ~prop:Prop.(!(
    ((3%"r1" = 1) && (3%"r2" = 2) && (4%"r3" = 2) && (4%"r4" = 1))
    ||
    ((3%"r1" = 2) && (3%"r2" = 1) && (4%"r3" = 1) && (4%"r4" = 2))
  ))
  prog_CoRR
)

let prog_IRIW = <:cppmem_par<
  spw {{{
    x_sc := 1
  |||
    y_sc := 1
  |||
    r1 := x_sc;
    r2 := y_sc
  |||
    r3 := y_sc;
    r4 := x_sc
  }}}
>>

let test_IRIW = Test.(make_test_desc
  ~name:"IRIW"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";"y";]
  ~kind:Safe
  ~prop:Prop.(!(
    ((3%"r1" = 1) && (3%"r2" = 0) && (4%"r3" = 1) && (4%"r4" = 0))
  ))
  prog_IRIW
)

let prog_WRC = <:cppmem_par<
  spw {{{
    x_sc := 1
  |||
    r1 := x_sc;
    y_sc := r1
  |||
    r2 := y_sc;
    r3 := x_sc
  }}}
>>

let test_WRC = Test.(make_test_desc
  ~name:"WRC"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";"y";]
  ~kind:Safe
  ~prop:Prop.(
    !((3%"r2" = 1) && (3%"r3" = 0))
  )
  prog_WRC
)

let tests_sc_op = Test.make_operational_testsuite ~model:Test.SeqCst [
  test_SW;
  test_SB;
  test_LB;
  test_MP;
  test_CoRR;
  test_IRIW;
  test_WRC;
]

(* let tests_sc_axiom = make_litmus_testsuite
  ~name:"SeqCst"
  ~tactic:Tactic.Sequential
  ~consistento:Axiomatic.SequentialConsistent.consistento
  (module Axiomatic.PreExecution)
  [
    test_SW_SC;
    test_SB_SC;
    test_LB_SC;
    test_MP_SC;
    test_CoRR_SC;
  ] *)

(* ************************************************************************** *)
(* ********************* TotalStoreOrder Tests ****************************** *)
(* ************************************************************************** *)

let prog_SW = <:cppmem_par<
  spw {{{
      x_rlx := 1;
      f_rlx := 1
  |||
      r1 := f_rlx;
      r2 := x_rlx
  }}}
>>

let test_SW = Test.(make_test_desc
  ~name:"SW"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~kind:Safe
  ~prop:Prop.(
       ((2%"r1" = 0) && (2%"r2" = 0))
    || ((2%"r1" = 0) && (2%"r2" = 1))
    || ((2%"r1" = 1) && (2%"r2" = 1))
  )
  prog_SW
)

let prog_SB = <:cppmem_par<
  spw {{{
      x_rlx := 1;
      r1 := y_rlx
  |||
      y_rlx := 1;
      r2 := x_rlx
  }}}
>>

let test_SB = Test.(make_test_desc
  ~name:"SB"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~kind:Unsafe
  ~prop:Prop.(
    !((1%"r1" = 0) && (2%"r2" = 0))
  )
  prog_SB
)

let prog_LB = <:cppmem_par<
    spw {{{
        r1 := x_rlx;
        y_rlx := 1
    |||
        r2 := y_rlx;
        x_rlx := 1
    }}}
>>

let test_LB = Test.(make_test_desc
  ~name:"LB"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~kind:Safe
  ~prop:Prop.(
    !((1%"r1" = 1) && (2%"r2" = 1))
  )
  prog_LB
)

let prog_MP = <:cppmem_par<
    spw {{{
        x_rlx := 1;
        f_rlx := 1
    |||
        repeat r1 := f_rlx until r1;
        r2 := x_rlx
    }}}
>>

let test_MP = Test.(make_test_desc
  ~name:"MP"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~kind:Safe
  ~prop:Prop.(
    (2%"r2" = 1)
  )
  prog_MP
)

let prog_CoRR = <:cppmem_par<
  spw {{{
    x_rlx := 1
  |||
    x_rlx := 2
  |||
    r1 := x_rlx;
    r2 := x_rlx
  |||
    r3 := x_rlx;
    r4 := x_rlx
  }}}
>>

let test_CoRR = Test.(make_test_desc
  ~name:"CoRR"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";]
  ~kind:Safe
  ~prop:Prop.(!(
    ((3%"r1" = 1) && (3%"r2" = 2) && (4%"r3" = 2) && (4%"r4" = 1))
    ||
    ((3%"r1" = 2) && (3%"r2" = 1) && (4%"r3" = 1) && (4%"r4" = 2))
  ))
  prog_CoRR
)

let prog_IRIW = <:cppmem_par<
  spw {{{
    x_rlx := 1
  |||
    y_rlx := 1
  |||
    r1 := x_rlx;
    r2 := y_rlx
  |||
    r3 := y_rlx;
    r4 := x_rlx
  }}}
>>

let test_IRIW = Test.(make_test_desc
  ~name:"IRIW"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";"y";]
  ~kind:Safe
  ~prop:Prop.(
    !((3%"r1" = 1) && (3%"r2" = 0) && (4%"r3" = 1) && (4%"r4" = 0))
  )
  prog_IRIW
)

let prog_WRC = <:cppmem_par<
  spw {{{
    x_rlx := 1
  |||
    r1 := x_rlx;
    y_rlx := r1
  |||
    r2 := y_rlx;
    r3 := x_rlx
  }}}
>>

let test_WRC = Test.(make_test_desc
  ~name:"WRC"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";"y";]
  ~kind:Safe
  ~prop:Prop.(
    !((3%"r2" = 1) && (3%"r3" = 0))
  )
  prog_WRC
)

let tests_tso_op = Test.make_operational_testsuite ~model:Test.TSO [
  test_SW;
  test_SB;
  test_LB;
  test_MP;
  test_CoRR;
  test_IRIW;
  test_WRC;
]

(* ************************************************************************** *)
(* ********************** ReleaseAcquire Tests ****************************** *)
(* ************************************************************************** *)

let prog_SW = <:cppmem_par<
  spw {{{
      x_rlx := 1;
      f_rel := 1
  |||
      r1 := f_acq;
      r2 := x_rlx
  }}}
>>

let test_SW = Test.(make_test_desc
  ~name:"SW"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~kind:Safe
  ~prop:Prop.(
       ((2%"r1" = 0) && (2%"r2" = 0))
    || ((2%"r1" = 0) && (2%"r2" = 1))
    || ((2%"r1" = 1) && (2%"r2" = 1))
  )
  prog_SW
)

let prog_SB = <:cppmem_par<
  spw {{{
      x_rel := 1;
      r1 := y_acq
  |||
      y_rel := 1;
      r2 := x_acq
  }}}
>>

let test_SB = Test.(make_test_desc
  ~name:"SB"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~kind:Unsafe
  ~prop:Prop.(
    !((1%"r1" = 0) || (2%"r2" = 0))
  )
  prog_SB
)

let prog_LB = <:cppmem_par<
    spw {{{
        r1 := x_acq;
        y_rel := 1
    |||
        r2 := y_acq;
        x_rel := 1
    }}}
>>

let test_LB = Test.(make_test_desc
  ~name:"LB+rel+acq"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~kind:Safe
  ~prop:Prop.(
    !((1%"r1" = 1) && (2%"r2" = 1))
  )
  prog_LB
)

let prog_LB_acq_rlx = <:cppmem_par<
    spw {{{
        r1 := x_acq;
        y_rlx := 1
    |||
        r2 := y_rlx;
        x_rel := 1
    }}}
>>

let test_LB_acq_rlx = Test.(make_test_desc
  ~name:"LB+acq+rlx"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"]
  ~kind:Safe
  ~prop:Prop.(
    !((1%"r1" = 1) && (2%"r2" = 1))
  )
  prog_LB_acq_rlx
)

let prog_MP = <:cppmem_par<
    spw {{{
        x_rlx := 1;
        f_rel := 1
    |||
        repeat r1 := f_acq until r1;
        r2 := x_rlx
    }}}
>>

let test_MP = Test.(make_test_desc
  ~name:"MP"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~kind:Safe
  ~prop:Prop.(
    (2%"r2" = 1)
  )
  prog_MP
)

let prog_MP_rlx_acq = <:cppmem_par<
    spw {{{
        x_rlx := 1;
        f_rlx := 1
    |||
        repeat r1 := f_acq until r1;
        r2 := x_rlx
    }}}
>>

let test_MP_rlx_acq = Test.(make_test_desc
  ~name:"MP+rlx+acq"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~kind:Unsafe
  ~prop:Prop.(
    !((2%"r2" = 0))
  )
  prog_MP_rlx_acq
)

let prog_MP_rel_rlx = <:cppmem_par<
    spw {{{
        x_rlx := 1;
        f_rel := 1
    |||
        repeat r1 := f_rlx until r1;
        r2 := x_rlx
    }}}
>>

let test_MP_rel_rlx = Test.(make_test_desc
  ~name:"MP+rel+rlx"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"; "f"]
  ~kind:Unsafe
  ~prop:Prop.(
    !(2%"r2" = 0)
  )
  prog_MP_rel_rlx
)

let prog_MP_relseq = <:cppmem_par<
  spw {{{
      x_rlx := 1;
      f_rel := 1;
      f_rlx := 2
  |||
      repeat r1 := f_acq until (r1 = 2);
      r2 := x_rlx
  }}}
>>

let test_MP_relseq = Test.(make_test_desc
  ~name:"MP+relseq"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~kind:Safe
  ~prop:Prop.(
    (2%"r2" = 1)
  )
  prog_MP_relseq
)

let prog_CoRR = <:cppmem_par<
  spw {{{
    x_rel := 1
  |||
    x_rel := 2
  |||
    r1 := x_acq;
    r2 := x_acq
  |||
    r3 := x_acq;
    r4 := x_acq
  }}}
>>

let test_CoRR = Test.(make_test_desc
  ~name:"CoRR"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";]
  ~kind:Safe
  ~len:Long
  ~prop:Prop.(!(
    ((3%"r1" = 1) && (3%"r2" = 2) && (4%"r3" = 2) && (4%"r4" = 1))
    ||
    ((3%"r1" = 2) && (3%"r2" = 1) && (4%"r3" = 1) && (4%"r4" = 2))
  ))
  prog_CoRR
)

let prog_IRIW = <:cppmem_par<
  spw {{{
    x_rel := 1
  |||
    y_rel := 1
  |||
    r1 := x_acq;
    r2 := y_acq
  |||
    r3 := y_acq;
    r4 := x_acq
  }}}
>>

let test_IRIW = Test.(make_test_desc
  ~name:"IRIW"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";"y";]
  ~kind:Unsafe
  ~prop:Prop.(
    !((3%"r1" = 1) && (3%"r2" = 0) && (4%"r3" = 1) && (4%"r4" = 0))
  )
  prog_IRIW
)

let prog_IRIW_rlx = <:cppmem_par<
  spw {{{
    x_rlx := 1
  |||
    y_rlx := 1
  |||
    r1 := x_rlx;
    r2 := y_rlx
  |||
    r3 := y_rlx;
    r4 := x_rlx
  }}}
>>

let test_IRIW_rlx = Test.(make_test_desc
  ~name:"IRIW+rlx"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";"y";]
  ~kind:Unsafe
  ~prop:Prop.(
    !((3%"r1" = 1) && (3%"r2" = 0) && (4%"r3" = 1) && (4%"r4" = 0))
  )
  prog_IRIW_rlx
)

let prog_WRC = <:cppmem_par<
  spw {{{
    x_rel := 1
  |||
    r1 := x_acq;
    y_rel := r1
  |||
    r2 := y_acq;
    r3 := x_acq
  }}}
>>

let test_WRC = Test.(make_test_desc
  ~name:"WRC"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";"y";]
  ~kind:Safe
  ~prop:Prop.(
    !((3%"r2" = 1) && (3%"r3" = 0))
  )
  prog_WRC
)

let prog_rlx_WRC = <:cppmem_par<
  spw {{{
    x_rlx := 1
  |||
    r1 := x_rlx;
    y_rlx := r1
  |||
    r2 := y_rlx;
    r3 := x_rlx
  }}}
>>

let test_WRC_rlx = Test.(make_test_desc
  ~name:"WRC+rlx"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";"y";]
  ~kind:Unsafe
  ~prop:Prop.(
    !((3%"r2" = 1) && (3%"r3" = 0))
  )
  prog_rlx_WRC
)

let prog_cas_WRC = <:cppmem_par<
  spw {{{
    x_rel := 1;
    y_rel := 1
  |||
    r2 := CAS(rel, acq, y, 1, 2)
  |||
    r3 := y_acq;
    r4 := x_acq
  }}}
>>

let test_WRC_cas = Test.(make_test_desc
  ~name:"WRC+cas"
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x"; "y";]
  ~kind:Safe
  ~prop:Prop.(
    !((3%"r3" = 2) && (3%"r4" = 0))
  )
  prog_cas_WRC
)

let prog_DR_WW = <:cppmem_par<
  spw {{{
      x_na := 1
  |||
      x_na := 2
  }}}
>>

let test_DR_WW = Test.(make_test_desc
  ~name:"DR_WW"
  ~regs:["r1"]
  ~locs:["x"]
  ~kind:Unsafe
  ~prop:Prop.(
    !(datarace ())
  )
  prog_DR_WW
)

let prog_DR_RW = <:cppmem_par<
  spw {{{
      x_na := 1
  |||
      r1 := x_na
  }}}
>>

let test_DR_RW = Test.(make_test_desc
  ~name:"DR_RW"
  ~regs:["r1"]
  ~locs:["x"]
  ~kind:Unsafe
  ~prop:Prop.(
    !(datarace ())
  )
  prog_DR_WW
)

let prog_DR_RW_rlxR = <:cppmem_par<
  spw {{{
      x_rlx := 1
  |||
      r1 := x_na
  }}}
>>

let test_DR_RW_rlxR = Test.(make_test_desc
  ~name:"DR_RW+rlxR"
  ~regs:["r1"]
  ~locs:["x"]
  ~kind:Unsafe
  ~prop:Prop.(
    !(datarace ())
  )
  prog_DR_RW_rlxR
)

let prog_DR_RW_rlxW = <:cppmem_par<
  spw {{{
      x_na := 1
  |||
      r1 := x_rlx
  }}}
>>

let test_DR_RW_rlxW = Test.(make_test_desc
  ~name:"DR_RW+rlxW"
  ~regs:["r1"]
  ~locs:["x"]
  ~kind:Unsafe
  ~prop:Prop.(
    !(datarace ())
  )
  prog_DR_RW_rlxW
)

let tests_ra_op = Test.make_operational_testsuite ~model:Test.RelAcq [
  test_SW;
  test_SB;
  test_LB;
  test_LB_acq_rlx;
  test_MP;
  test_MP_rlx_acq;
  test_MP_rel_rlx;
  test_MP_relseq;
  test_CoRR;
  test_IRIW;
  test_IRIW_rlx;
  test_WRC;
  test_WRC_rlx;
  test_WRC_cas;
  test_DR_WW;
  test_DR_RW;
  test_DR_RW_rlxW;
  test_DR_RW_rlxR;
]

let tests = Test.(
  make_testsuite ~name:"Litmus" [
    make_testsuite ~name:"Operational" [
      tests_sc_op;
      tests_tso_op;
      tests_ra_op;
    ];

    make_testsuite ~name:"Axiomatic" [
      (* tests_sc_axiom; *)
    ]
  ]
)
