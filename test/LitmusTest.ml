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

type memory_model = SC | RelAcq

type quantifier = Exists | Forall

type assertion = Safe | Datarace | RegsAssert of (RegStorage.ti -> MiniKanren.goal)

type litmus_test_desc =
  { name      : string
  ; prog      : CProg.ti
  ; regs      : string list
  ; locs      : string list
  ; quant     : quantifier
  ; assrt     : assertion
  }

let litmus_test ~name ~prog ~regs ~locs ~quant ~assrt =
  {name; prog; regs; locs; quant; assrt }

let test_exists ~name ~prog ~evalo ~terminatedo ~asserto ~initstate ~pprint =
  (* TODO: problem with tabling - we should not keep table between different runs *)
  (* if Stream.is_empty @@ Query.exec intrpo prog initstate then
    begin
      Format.fprintf ff "Litmus test %s is not runnable:@;" name;
      Test.Fail ""
    end
  else *)
    let po t = (terminatedo t) &&& (asserto t) in
    let stream = Query.eval (evalo ~po) initstate in
    if not @@ Stream.is_empty stream then
      Test.Ok
    else begin
      Format.printf "Litmus test %s fails!@;" name;
      let outs = Query.eval (evalo ~po:terminatedo) initstate in
      Format.printf "List of outputs:@;";
      Stream.iter (fun out -> Format.printf "%a@;" pprint out) outs;
      Test.Fail ""
    end

let test_forall ~name ~prog ~evalo ~terminatedo ~asserto ~initstate ~pprint =
  (* check that test is runnable *)
  (* if Stream.is_empty @@ Query.exec interpo prog initstate then
    begin
      Format.printf "Litmus test is not runnable@;";
      Test.Fail ""
    end
  else *)
    let po t = (terminatedo t) &&& (asserto t) in
    let stream = Query.eval (evalo ~po) initstate in
    if Stream.is_empty stream then
      Test.Ok
    else begin
      Format.printf "Litmus test %s fails!@;" name;
      let cexs = Stream.take stream in
      Format.printf "List of counterexamples:@;";
      (* input is irrelevant for litmus test, we show only final state *)
      List.iter (fun cex -> Format.printf "%a@;" pprint cex) cexs;
      Test.Fail ""
    end

let make_litmus_testcase
  (type a)
  (type b)
  ?(consistento: ((a, b logic) injected -> goal) = fun _ -> success)
  ~tactic
  (module Memory: MemoryModel with type tt = a and type inner = b)
  {name; prog; regs; locs; quant; assrt}
  =
  let module Interpreter = ConcurrentInterpreter(Memory) in
  let module Trace = Utils.Trace(Interpreter.ProgramState) in
  let thrdn = thrdnum prog in
  let mem = Memory.alloc ~thrdn locs in
  let regs = RegStorage.init thrdn @@ Regs.alloc regs in
  let initstate = Interpreter.(ProgramState.make prog @@ State.init regs mem) in
  let terminatedo t =
    fresh (s m)
      (Interpreter.ProgramState.terminatedo t)
      (Interpreter.ProgramState.stateo t s)
      (Interpreter.State.memo s m)
      (consistento m)
  in
  let state_asserto = match assrt with
  | Safe          -> Interpreter.State.safeo
  | Datarace      -> Interpreter.State.dataraceo
  | RegsAssert g  -> fun s ->
    fresh (r)
      (Interpreter.State.regstorageo s r)
      (g r)
  in
  let asserto t =
    fresh (s)
      (Interpreter.ProgramState.stateo t s)
      (match quant with
        | Exists ->    state_asserto s
        | Forall -> ?~(state_asserto s)
      )
  in
  let evalo = Interpreter.evalo ~tactic in
  let test_fun = match quant with
  | Exists -> test_exists
  | Forall -> test_forall
  in
  let pprint = Trace.trace in
  let test () = test_fun ~name ~prog ~evalo ~terminatedo ~asserto ~initstate ~pprint in
  Test.make_testcase ~name ~test

let make_litmus_testsuite
  (type a)
  (type b)
  ?(consistento: ((a, b logic) injected -> goal) option)
  ~name
  ~tactic
  (module Memory: MemoryModel with type tt = a and type inner = b)
  descs
  =
  let tests = List.map (make_litmus_testcase ?consistento ~tactic (module Memory)) descs in
  Test.make_testsuite ~name ~tests

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs)
      (RegStorage.geto regs (ThreadID.tid 2) rs)
      (conde
        [ Regs.checko rs [("r1", 0); ("r2", 0)]
        ; Regs.checko rs [("r1", 0); ("r2", 1)]
        ; Regs.checko rs [("r1", 1); ("r2", 1)]
        ]
      )
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs1 rs2)
      (RegStorage.geto regs (ThreadID.tid 1) rs1)
      (RegStorage.geto regs (ThreadID.tid 2) rs2)
    ?~((Regs.checko rs1 [("r1", 0)]) &&& (Regs.checko rs2 [("r2", 0)]))
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs1 rs2)
      (RegStorage.geto regs (ThreadID.tid 1) rs1)
      (RegStorage.geto regs (ThreadID.tid 2) rs2)
    ?~((Regs.checko rs1 [("r1", 1)]) &&& (Regs.checko rs2 [("r2", 1)]))
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs)
      (RegStorage.geto regs (ThreadID.tid 2) rs)
      (Regs.checko rs [("r2", 1)])
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs3 rs4)
      (RegStorage.geto regs (ThreadID.tid 3) rs3)
      (RegStorage.geto regs (ThreadID.tid 4) rs4)
    ?~(conde
        [ (Regs.checko rs3 [("r1", 1); ("r2", 2)]) &&& (Regs.checko rs4 [("r3", 2); ("r4", 1)])
        ; (Regs.checko rs3 [("r1", 2); ("r2", 1)]) &&& (Regs.checko rs4 [("r3", 1); ("r4", 2)])
        ]
      )
  ))

let tests_sc_op = make_litmus_testsuite
  ~name:"SeqCst"
  ~tactic:Tactic.Interleaving
  (module Operational.SequentialConsistent)
  [
    test_SW_SC;
    test_SB_SC;
    test_LB_SC;
    test_MP_SC;
    test_CoRR_SC;
  ]

let tests_sc_axiom = make_litmus_testsuite
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

let test_SW_RA = litmus_test
  ~name:"SW_RA"
  ~prog:prog_SW
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Forall
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs)
      (RegStorage.geto regs (ThreadID.tid 2) rs)
      (conde
        [ Regs.checko rs [("r1", 0); ("r2", 0)]
        ; Regs.checko rs [("r1", 0); ("r2", 1)]
        ; Regs.checko rs [("r1", 1); ("r2", 1)]
        ]
      )
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs1 rs2)
      (RegStorage.geto regs (ThreadID.tid 1) rs1)
      (RegStorage.geto regs (ThreadID.tid 2) rs2)
      (Regs.checko rs1 [("r1", 0)])
      (Regs.checko rs2 [("r2", 0)])
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs1 rs2)
      (RegStorage.geto regs (ThreadID.tid 1) rs1)
      (RegStorage.geto regs (ThreadID.tid 2) rs2)
    ?~((Regs.checko rs1 [("r1", 1)]) &&& (Regs.checko rs2 [("r2", 1)]))
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs1 rs2)
      (RegStorage.geto regs (ThreadID.tid 1) rs1)
      (RegStorage.geto regs (ThreadID.tid 2) rs2)
    ?~((Regs.checko rs1 [("r1", 1)]) &&& (Regs.checko rs2 [("r2", 1)]))
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs)
      (RegStorage.geto regs (ThreadID.tid 2) rs)
      (Regs.checko rs [("r2", 1)])
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs)
      (RegStorage.geto regs (ThreadID.tid 2) rs)
      (Regs.checko rs [("r2", 0)])
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs)
      (RegStorage.geto regs (ThreadID.tid 2) rs)
      (Regs.checko rs [("r2", 0)])
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs)
      (RegStorage.geto regs (ThreadID.tid 2) rs)
      (Regs.checko rs [("r2", 1)])
  ))

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
  ~assrt:(RegsAssert (fun regs ->
    fresh (rs3 rs4)
      (RegStorage.geto regs (ThreadID.tid 3) rs3)
      (RegStorage.geto regs (ThreadID.tid 4) rs4)
    ?~(conde
        [ (Regs.checko rs3 [("r1", 1); ("r2", 2)]) &&& (Regs.checko rs4 [("r3", 2); ("r4", 1)])
        ; (Regs.checko rs3 [("r1", 2); ("r2", 1)]) &&& (Regs.checko rs4 [("r3", 1); ("r4", 2)])
        ]
      )
  ))

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
  ~assrt:Datarace

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
  ~assrt:Datarace

let tests_ra_op = make_litmus_testsuite
  ~name:"RelAcq"
  ~tactic:Tactic.Interleaving
  (module Operational.ReleaseAcquire)
  [
    test_SW_RA;
    test_SB_RA;
    test_LB_RA;
    test_LB_acq_rlx_RA;
    test_MP_RA;
    test_MP_rlx_acq_RA;
    test_MP_rel_rlx_RA;
    test_MP_relseq_RA;
    test_CoRR_RA;
    test_DR1_RA;
    test_DR2_RA;
  ]

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
