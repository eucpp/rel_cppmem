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

type assertion = Safe | Datarace

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

let test_exists ~name ~prog ~interpo ~initstate ~asserto ~pprint =
  (* TODO: problem with tabling - we should not keep table between different runs *)
  (* if Stream.is_empty @@ Query.exec intrpo prog initstate then
    begin
      Format.fprintf ff "Litmus test %s is not runnable:@;" name;
      Test.Fail ""
    end
  else *)
    let inputo s = (s === initstate) in
    let stream = Query.angelic interpo inputo asserto prog in
    if not @@ Stream.is_empty stream then
      Test.Ok
    else begin
      Format.printf "Litmus test %s fails!@;" name;
      let outs = Query.exec interpo prog initstate in
      Format.printf "List of outputs:@;";
      (* input is irrelevant for litmus test, we show only final state *)
      Stream.iter (fun out -> Format.printf "%a@;" pprint out) outs;
      Test.Fail ""
    end

let test_forall ~name ~prog ~interpo ~initstate ~asserto ~pprint =
  (* check that test is runnable *)
  if Stream.is_empty @@ Query.exec interpo prog initstate then
    begin
      Format.printf "Litmus test is not runnable:@;";
      Test.Fail ""
    end
  else
    let inputo s = (s === initstate) in
    let asserto _ = asserto in
    let stream = Query.verify ~interpo ~asserto inputo prog in
    if Stream.is_empty stream then
      Test.Ok
    else begin
      Format.printf "Litmus test %s fails!@;" name;
      let cexs = Stream.take stream in
      Format.printf "List of counterexamples:@;";
      (* input is irrelevant for litmus test, we show only final state *)
      List.iter (fun (_, cex) -> Format.printf "%a@;" pprint cex) cexs;
      Test.Fail ""
    end

let make_litmus ~tactic (module Memory: MemoryModel) {name; prog; regs; locs; quant; assrt} =
  let module Interpreter = ConcurrentInterpreter(Memory) in
  let module Trace = Utils.Trace(Interpreter.State) in

  let thrdn = thrdnum prog in
  let mem = Memory.alloc ~thrdn locs in
  let regs = RegStorage.init thrdn @@ Regs.alloc regs in
  let initstate = Interpreter.State.init regs mem in

  let interpo = Interpreter.interpo tactic in
  let asserto = match assrt with
  | Safe      -> Interpreter.State.safeo
  | Datarace  -> Interpreter.State.dataraceo
  in
  let pprint = Trace.trace in

  let test_fun = match quant with
  | Exists -> test_exists
  | Forall -> test_forall
  in
  let test () = test_fun ~name ~prog ~interpo ~initstate ~asserto ~pprint in

  Test.make_testcase ~name ~test

(* ************************************************************************** *)
(* ******************* SequentialConsistent Tests *************************** *)
(* ************************************************************************** *)

(* let safeo_SC s =
  let module Interpreter = ConcurrentInterpreter(Operational.SequentialConsistent) in
  Interpreter.State.erroro s ~sg:(fun _ -> failure) ~fg:(success) *)

let prog_SW = <:cppmem_par<
  spw {{{
      x_sc := 1;
      f_sc := 1
  |||
      r1 := f_sc;
      r2 := x_sc;
      assert (
        (r1 = 0 && r2 = 0) ||
        (r1 = 0 && r2 = 1) ||
        (r1 = 1 && r2 = 1)
      )
  }}}
>>

let test_SW_SC = litmus_test
  ~name:"SW_SC"
  ~prog:prog_SW
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Forall
  ~assrt:Safe

(*
let prog_SB = <:cppmem_par<
  spw {{{
      x_sc := 1;
      r1 := y_sc;
      return (r1)
  |||
      y_sc := 1;
      r2 := x_sc;
      return (r2)
  }}};
  assert (r1 != 0 || r2 != 0)
>>

let test_SB_SC = litmus_test
  ~name:"SB_SC"
  ~prog:prog_SB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~quant:Forall
  ~assrt:Safe

let prog_LB = <:cppmem_par<
    spw {{{
        r1 := x_sc;
        y_sc := 1;
        return (r1)
    |||
        r2 := y_sc;
        x_sc := 1;
        return (r2)
    }}};
    assert (r1 != 1 || r2 != 1)
>>

let test_LB_SC = litmus_test
  ~name:"LB_SC"
  ~prog:prog_LB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~quant:Forall
  ~assrt:Safe

let prog_MP = <:cppmem_par<
    spw {{{
        x_sc := 1;
        f_sc := 1
    |||
        repeat r1 := f_sc until r1;
        r2 := x_sc;
        return (r2)
    }}};
    assert (r2 = 1)
>>

let test_MP_SC = litmus_test
  ~name:"MP_SC"
  ~prog:prog_MP
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Forall
  ~assrt:Safe

let prog_CoRR = <:cppmem_par<
  spw {{{
    spw {{{
      x_sc := 1
    |||
      x_sc := 2
    }}}
  |||
    spw {{{
      r1 := x_sc;
      r2 := x_sc;
      return (r1, r2)
    |||
      r3 := x_sc;
      r4 := x_sc;
      return (r3, r4)
    }}};
    assert (
      !(((r1 = 1) && (r2 = 2) && (r3 = 2) && (r4 = 1))
        ||
        ((r1 = 2) && (r2 = 1) && (r3 = 1) && (r4 = 2))
      )
    )
  }}}
>>

let test_CoRR_SC = litmus_test
  ~name:"CoRR_SC"
  ~prog:prog_CoRR
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";]
  ~quant:Forall
  ~assrt:Safe
*)

let tests_sc_op =
  let mem_model = (module Operational.SequentialConsistent: MemoryModel) in
  let make_tests = List.map (make_litmus ~tactic:Interleaving mem_model) in
  Test.make_testsuite ~name:"SeqCst" ~tests: (make_tests [
    test_SW_SC;
    (* test_SB_SC;
    test_LB_SC;
    test_MP_SC;
    test_CoRR_SC; *)
  ])

(* ************************************************************************** *)
(* ********************** ReleaseAcquire Tests ****************************** *)
(* ************************************************************************** *)

let prog_SW = <:cppmem_par<
  spw {{{
      x_rlx := 1;
      f_rel := 1
  |||
      r1 := f_acq;
      r2 := x_rlx;
      assert (
        (r1 = 0 && r2 = 0) ||
        (r1 = 0 && r2 = 1) ||
        (r1 = 1 && r2 = 1)
      )
  }}}
>>

let test_SW_RA = litmus_test
  ~name:"SW_RA"
  ~prog:prog_SW
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Forall
  ~assrt:Safe

(*
let prog_SB = <:cppmem_par<
  spw {{{
      x_rel := 1;
      r1 := y_acq;
      return (r1)
  |||
      y_rel := 1;
      r2 := x_acq;
      return (r2)
  }}};
  assert (r1 = 0 && r2 = 0)
>>

let test_SB_RA = litmus_test
  ~name:"SB_RA"
  ~prog:prog_SB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~quant:Exists
  ~assrt:Safe

let prog_LB = <:cppmem_par<
    spw {{{
        r1 := x_acq;
        y_rel := 1;
        return (r1)
    |||
        r2 := y_acq;
        x_rel := 1;
        return (r2)
    }}};
    assert (r1 != 1 || r2 != 1)
>>

let test_LB_RA = litmus_test
  ~name:"LB+rel+acq_RA"
  ~prog:prog_LB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~quant:Forall
  ~assrt:Safe

let prog_LB_acq_rlx = <:cppmem_par<
    spw {{{
        r1 := x_acq;
        y_rlx := 1;
        return (r1)
    |||
        r2 := y_rlx;
        x_rel := 1;
        return (r2)
    }}};
    assert (r1 != 1 || r2 != 1)
>>

let test_LB_acq_rlx_RA = litmus_test
  ~name:"LB+acq+rlx_RA"
  ~prog:prog_LB_acq_rlx
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"]
  ~quant:Forall
  ~assrt:Safe
*)

let prog_MP = <:cppmem_par<
    spw {{{
        x_rlx := 1;
        f_rel := 1
    |||
        repeat r1 := f_acq until r1;
        r2 := x_rlx;
        assert (r2 = 1)
    }}}
>>

let test_MP_RA = litmus_test
  ~name:"MP_RA"
  ~prog:prog_MP
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Forall
  ~assrt:Safe

let prog_MP_rlx_acq = <:cppmem_par<
    spw {{{
        x_rlx := 1;
        f_rlx := 1
    |||
        repeat r1 := f_acq until r1;
        r2 := x_rlx;
        assert (r2 = 0)
    }}}
>>

let test_MP_rlx_acq_RA = litmus_test
  ~name:"MP+rlx+acq_RA"
  ~prog:prog_MP_rlx_acq
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Exists
  ~assrt:Safe

let prog_MP_rel_rlx = <:cppmem_par<
    spw {{{
        x_rlx := 1;
        f_rel := 1
    |||
        repeat r1 := f_rlx until r1;
        r2 := x_rlx;
        assert (r2 = 0)
    }}}
>>

let test_MP_rel_rlx_RA = litmus_test
  ~name:"MP+rel+rlx_RA"
  ~prog:prog_MP_rel_rlx
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"; "f"]
  ~quant:Exists
  ~assrt:Safe

let prog_MP_relseq = <:cppmem_par<
  spw {{{
      x_rlx := 1;
      f_rel := 1;
      f_rlx := 2
  |||
      repeat r1 := f_acq until (r1 = 2);
      r2 := x_rlx;
      assert (r2 = 1)
  }}}
>>

let test_MP_relseq_RA = litmus_test
  ~name:"MP+relseq_RA"
  ~prog:prog_MP_relseq
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~quant:Forall
  ~assrt:Safe

(*
let prog_CoRR = <:cppmem_par<
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
      return (r1, r2)
    |||
      r3 := x_rlx;
      r4 := x_rlx;
      return (r3, r4)
    }}};
    assert (
      !(((r1 = 1) && (r2 = 2) && (r3 = 2) && (r4 = 1))
        ||
        ((r1 = 2) && (r2 = 1) && (r3 = 1) && (r4 = 2))
      )
    )
  }}}
>>


let test_CoRR_RA = litmus_test
  ~name:"CoRR_RA"
  ~prog:prog_CoRR
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";]
  ~quant:Forall
  ~assrt:Safe
*)

let prog_DR1 = <:cppmem_par<
  spw {{{
      x_rlx := 1
  |||
      r1 := x_na;
      assert (r1 = 0)
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
      r1 := x_rlx;
      assert (r1 = 0)
  }}}
>>

let test_DR2_RA = litmus_test
  ~name:"DR2_RA"
  ~prog:prog_DR2
  ~regs:["r1"]
  ~locs:["x"]
  ~quant:Exists
  ~assrt:Datarace

let tests_ra_op =
  let mem_model = (module Operational.ReleaseAcquire: MemoryModel) in
  let make_tests = List.map (make_litmus ~tactic:Interleaving mem_model) in
  Test.make_testsuite ~name:"RelAcq" ~tests: (make_tests [
    test_SW_RA;
    (* test_SB_RA;
    test_LB_RA;
    test_LB_acq_rlx_RA; *)
    test_MP_RA;
    test_MP_rlx_acq_RA;
    test_MP_rel_rlx_RA;
    test_MP_relseq_RA;
    (* test_CoRR_RA; *)
    test_DR1_RA;
    test_DR2_RA;
  ])

let tests = Test.(
  make_testsuite ~name:"Litmus" ~tests: [
    make_testsuite ~name:"Operational" ~tests: [
      tests_sc_op;
      tests_ra_op;
    ]
  ]
)

(* let tests =
  "Litmus">::: [
    "RelAcq">::: List.map Test.ounit_test [
      test_SW_RA;
      test_SB_RA;
      test_LB_RA;
      test_DR1_RA;
      test_DR2_RA;
      test_LB_RA_rlx;
      test_MP_RA;
      test_MP_RA_rlx1;
      test_MP_RA_rlx2;
      test_MP_RA_rel_seq;
      test_MP_CoRR;
    ] *)

    (* "DR_1">:: test_data_race_1 rlxStep;
    "DR_2">:: test_data_race_2 rlxStep;

    "rel_acq">:: test_rel_acq relAcqStep;
    "SB">:: test_SB relAcqStep;
    "LB_rel_acq">:: test_LB_rel_acq relAcqStep;
    "LB_rel_acq_rlx">:: test_LB_rel_acq_rlx relAcqStep;
    "MP">:: test_MP relAcqStep;
    "MP_rlx_1">:: test_MP_rlx_1 relAcqStep;
    "MP_rlx_2">:: test_MP_rlx_2 relAcqStep;
    "MP_rel_seq">:: test_MP_rel_seq relAcqStep;
    (* "CoRR_rlx">: OUnitTest.TestCase (OUnitTest.Long, test_CoRR_rlx relAcqStep); *)

    "SB_sc">:: test_SB_sc scStep;

    "LB">:: test_LB promisingStep;
    "LBd">:: test_LBd promisingStep;
  ]*)
