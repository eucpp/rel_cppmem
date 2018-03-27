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

type memory_model = SeqCst | RelAcq

type quantifier = Exists | Forall

type litmus_test_desc =
  { name      : string
  ; prog      : Prog.ti list
  ; regs      : string list
  ; locs      : string list
  ; quant     : quantifier
  ; prop      : Prop.ti
  }

let litmus_test ~name ~prog ~regs ~locs ~quant ~prop =
  { name; prog; regs; locs; quant; prop }

module OperationalLitmusTest(Memory : Operational.MemoryModel.T) =
  struct
    module Interpreter = Operational.Interpreter(Memory)
    module Trace = Utils.Trace(Interpreter.State)

    let make_initstate ~prog ~regs ~locs =
      let thrdn = List.length prog in
      let tm = ThreadManager.init prog @@ Utils.repeat (Regs.alloc regs) thrdn in
      let mem = Memory.alloc ~thrdn locs in
      Interpreter.(State.init tm mem)

    let test_exists ~name ~prop ~prog ~regs ~locs =
      let istate = make_initstate ~prog ~regs ~locs in
      let stream = Interpreter.eval ~prop istate in
      if not @@ Stream.is_empty stream then
        Test.Ok
      else
        let outs = Interpreter.eval istate in
        Format.printf "Litmus test %s fails!@;" name;
        Format.printf "List of outputs:@;";
        Stream.iter (fun out -> Format.printf "%a@;" Trace.trace out) outs;
        Test.Fail ""

    let test_forall ~name ~prop ~prog ~regs ~locs =
      let istate = make_initstate ~prog ~regs ~locs in
      let stream = Interpreter.eval ~prop:(Prop.neg prop) istate in
      if Stream.is_empty stream then
        Test.Ok
      else begin
        let cexs = Stream.take stream in
        Format.printf "Litmus test %s fails!@;" name;
        Format.printf "List of counterexamples:@;";
        List.iter (fun cex -> Format.printf "%a@;" Trace.trace cex) cexs;
        Test.Fail ""
      end

    let make_testcase {name; prog; prop; regs; locs; quant} =
      let test () =
        match quant with
        | Exists -> test_exists ~name ~prop ~prog ~regs ~locs
        | Forall -> test_forall ~name ~prop ~prog ~regs ~locs
      in
      Test.make_testcase ~name ~test
  end

let make_litmus_testsuite ~name ~tests (module Memory: Operational.MemoryModel.T) =
  let module LitmusTest = OperationalLitmusTest(Memory) in
  Test.make_testsuite ~name ~tests:(List.map LitmusTest.make_testcase tests)

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

let test_SW_SC = litmus_test
  ~name:"SW_SC"
  ~prog:prog_SW
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Forall
  ~prop:Prop.(
       ((2%"r1" = 0) && (2%"r2" = 0))
    || ((2%"r1" = 0) && (2%"r2" = 1))
    || ((2%"r1" = 1) && (2%"r2" = 1))
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

let test_SB_SC = litmus_test
  ~name:"SB_SC"
  ~prog:prog_SB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~quant:Forall
  ~prop:Prop.(
    !((1%"r1" = 0) && (2%"r2" = 0))
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

let test_LB_SC = litmus_test
  ~name:"LB_SC"
  ~prog:prog_LB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~quant:Forall
  ~prop:Prop.(
    !((1%"r1" = 1) && (2%"r2" = 1))
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

let test_MP_SC = litmus_test
  ~name:"MP_SC"
  ~prog:prog_MP
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Forall
  ~prop:Prop.(
    (2%"r2" = 1)
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

let test_CoRR_SC = litmus_test
  ~name:"CoRR_SC"
  ~prog:prog_CoRR
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";]
  ~quant:Forall
  ~prop:Prop.(!(
    ((3%"r1" = 1) && (3%"r2" = 2) && (4%"r3" = 2) && (4%"r4" = 1))
    ||
    ((3%"r1" = 2) && (3%"r2" = 1) && (4%"r3" = 1) && (4%"r4" = 2))
  ))

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

let test_IRIW_SC = litmus_test
  ~name:"IRIW_SC"
  ~prog:prog_IRIW
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";"y";]
  ~quant:Forall
  ~prop:Prop.(!(
    ((3%"r1" = 1) && (3%"r2" = 0) && (4%"r3" = 1) && (4%"r4" = 0))
  ))

let tests_sc_op = make_litmus_testsuite
  ~name:"SeqCst"
  ~tests:([
    test_SW_SC;
    test_SB_SC;
    test_LB_SC;
    test_MP_SC;
    test_CoRR_SC;
    test_IRIW_SC;
  ])
  (module Operational.SeqCst)

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

let test_SW_RA = litmus_test
  ~name:"SW_RA"
  ~prog:prog_SW
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Forall
  ~prop:Prop.(
       ((2%"r1" = 0) && (2%"r2" = 0))
    || ((2%"r1" = 0) && (2%"r2" = 1))
    || ((2%"r1" = 1) && (2%"r2" = 1))
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

let test_SB_RA = litmus_test
  ~name:"SB_RA"
  ~prog:prog_SB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~quant:Exists
  ~prop:Prop.(
    (1%"r1" = 0) || (2%"r2" = 0)
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

let test_LB_RA = litmus_test
  ~name:"LB+rel+acq_RA"
  ~prog:prog_LB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~quant:Forall
  ~prop:Prop.(
    !((1%"r1" = 1) && (2%"r2" = 1))
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

let test_LB_acq_rlx_RA = litmus_test
  ~name:"LB+acq+rlx_RA"
  ~prog:prog_LB_acq_rlx
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"]
  ~quant:Forall
  ~prop:Prop.(
    !((1%"r1" = 1) && (2%"r2" = 1))
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

let test_MP_RA = litmus_test
  ~name:"MP_RA"
  ~prog:prog_MP
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Forall
  ~prop:Prop.(
    (2%"r2" = 1)
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

let test_MP_rlx_acq_RA = litmus_test
  ~name:"MP+rlx+acq_RA"
  ~prog:prog_MP_rlx_acq
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Exists
  ~prop:Prop.(
    (2%"r2" = 0)
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

let test_MP_rel_rlx_RA = litmus_test
  ~name:"MP+rel+rlx_RA"
  ~prog:prog_MP_rel_rlx
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"; "f"]
  ~quant:Exists
  ~prop:Prop.(
    (2%"r2" = 0)
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

let test_MP_relseq_RA = litmus_test
  ~name:"MP+relseq_RA"
  ~prog:prog_MP_relseq
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Forall
  ~prop:Prop.(
    (2%"r2" = 1)
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

let test_CoRR_RA = litmus_test
  ~name:"CoRR_RA"
  ~prog:prog_CoRR
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";]
  ~quant:Forall
  ~prop:Prop.(!(
    ((3%"r1" = 1) && (3%"r2" = 2) && (4%"r3" = 2) && (4%"r4" = 1))
    ||
    ((3%"r1" = 2) && (3%"r2" = 1) && (4%"r3" = 1) && (4%"r4" = 2))
  ))

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

let test_IRIW_RA = litmus_test
  ~name:"IRIW_SC"
  ~prog:prog_IRIW
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";"y";]
  ~quant:Exists
  ~prop:Prop.(
    ((3%"r1" = 1) && (3%"r2" = 0) && (4%"r3" = 1) && (4%"r4" = 0))
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

let test_IRIW_rlx_RA = litmus_test
  ~name:"IRIW_SC"
  ~prog:prog_IRIW_rlx
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";"y";]
  ~quant:Exists
  ~prop:Prop.(
    ((3%"r1" = 1) && (3%"r2" = 0) && (4%"r3" = 1) && (4%"r4" = 0))
  )

let prog_DR1 = <:cppmem_par<
  spw {{{
      x_rlx := 1
  |||
      r1 := x_na
  }}}
>>

let test_DR1_RA = litmus_test
  ~name:"DR1_RA"
  ~prog:prog_DR1
  ~regs:["r1"]
  ~locs:["x"]
  ~quant:Exists
  ~prop:Prop.(
    datarace ()
  )

let prog_DR2 = <:cppmem_par<
  spw {{{
      x_na := 1
  |||
      r1 := x_rlx
  }}}
>>

let test_DR2_RA = litmus_test
  ~name:"DR2_RA"
  ~prog:prog_DR2
  ~regs:["r1"]
  ~locs:["x"]
  ~quant:Exists
  ~prop:Prop.(
    datarace ()
  )

let tests_ra_op = make_litmus_testsuite
  ~name:"RelAcq"
  ~tests:([
    test_SW_RA;
    test_SB_RA;
    test_LB_RA;
    test_LB_acq_rlx_RA;
    test_MP_RA;
    test_MP_rlx_acq_RA;
    test_MP_rel_rlx_RA;
    test_MP_relseq_RA;
    test_CoRR_RA;
    test_IRIW_RA;
    test_IRIW_rlx_RA;
    test_DR1_RA;
    test_DR2_RA;
  ])
  (module Operational.RelAcq)

let tests = Test.(
  make_testsuite ~name:"Litmus" ~tests: [
    make_testsuite ~name:"Operational" ~tests: [
      tests_sc_op;
      tests_ra_op;
    ];

    make_testsuite ~name:"Axiomatic" ~tests: [
      (* tests_sc_axiom; *)
    ]
  ]
)
