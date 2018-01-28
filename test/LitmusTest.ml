open MiniKanren
open MiniKanren.Std

open Lang
open Lang.Expr
open Lang.Stmt
open Lang.Loc
open Lang.Reg
open Lang.Value

type quantifier = Exists | Forall

type assertion = Safe | Datarace

type litmus_test =
  { name  : string
  ; prog  : Prog.ti list
  ; mem   : module MemoryModel
  ; regs  : string list
  ; locs  : string list
  ; quant : quantifier
  ; assrt : assertion
  }

module Tester =
  struct
    let test_exists ~name ~prog ~interpo ~inputo ~asserto ~pprint =
      (* TODO: problem with tabling - we should not keep table between different runs *)
      (* if Stream.is_empty @@ Query.exec intrpo prog initstate then
        begin
          Format.fprintf ff "Litmus test %s is not runnable:@;" name;
          Test.Fail ""
        end
      else *)
        let stream = Query.angelic interpo inputo asserto prog in
        if not @@ Stream.is_empty stream then
          Test.Ok
        else begin
          Format.printf "Litmus test %s fails!@;" name;
          begin match pprint with
            | Some pprint ->
              let outs = Query.exec interpo prog initstate in
              Format.printf "List of outputs:@;";
              (* input is irrelevant for litmus test, we show only final state *)
              Stream.iter (fun out -> Format.printf "%a@;" pprint out) outs
            | None -> ()
          end;
          Test.Fail ""
        end

    let test_forall ~name ~prog ~interpo ~inputo ~asserto ~pprint =
      (* check that test is runnable *)
      if Stream.is_empty @@ Query.exec interpo prog initstate then
        begin
          Format.printf "Litmus test is not runnable:@;";
          Test.Fail ""
        end
      else
        let asserto _ output = asserto output in
        let stream = Query.verify ~interpo ~asserto inputo prog in
        if Stream.is_empty stream then
          Test.Ok
        else begin
          Format.printf "Litmus test %s fails!@;" name;
          begin match pprint with
            | Some pprint ->
              let cexs = Stream.take stream in
              Format.printf "List of counterexamples:@;";
              (* input is irrelevant for litmus test, we show only final state *)
              List.iter (fun (_, cex) -> Format.printf "%a@;" pprint cex) cexs
            | None -> ()
          end;
          Test.Fail ""
        end

    let make_op_test {name; prog; mem_model; regs; locs; quant; assrt} =
      let module Interpreter = ConcurrentInterpreter(mem_model) in
      let module Trace = Trace(Interpreter.State) in

      let thrdn = List.length prog in
      let mem = Memory.alloc ~thrdn locs in
      let regs = RegStorage.init thrdn @@ Regs.alloc regs in
      let initstate = Interpreter.State.init regs mem in

      let prog = cprog prog in
      let interpo = Interpreter.interpo Interleaving in
      let inputo s = (s === initstate)
      let asserto _ = match assrt with
      | Safe      -> Interpreter.State.safeo
      | Datarace  -> Interpreter.State.dataraceo
      in
      let pprint = Trace.trace in

      let test_fun = match quant with
      | Exists -> test_exists
      | Forall -> test_forall
      in
      let test () = test_fun ~prog ~interpo ~inputo ~asserto ~pprint in

      Test.make_testcase ~name ~test

  end

(* ************************************************************************** *)
(* ******************* SequentialConsistent Tests *************************** *)
(* ************************************************************************** *)

module Litmus = MakeLitmus(Operational.SequentialConsistent)

(* let safeo_SC s =
  let module Interpreter = ConcurrentInterpreter(Operational.SequentialConsistent) in
  Interpreter.State.erroro s ~sg:(fun _ -> failure) ~fg:(success) *)

let prog_SW = <:cppmem<
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

let test_SW_SC = Litmus.test_forall
  ~name:"SW_SC"
  ~prog:prog_SW
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~asserto:State.safeo

let prog_SB = <:cppmem<
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

let test_SB_SC = Litmus.test_forall
  ~name:"SB_SC"
  ~prog:prog_SB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~asserto:State.safeo

let prog_LB = <:cppmem<
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

let test_LB_SC = Litmus.test_forall
  ~name:"LB_SC"
  ~prog:prog_LB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~asserto:State.safeo

let prog_MP = <:cppmem<
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

let test_MP_SC = Litmus.test_forall
  ~name:"MP_SC"
  ~prog:prog_MP
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~asserto:State.safeo

let prog_CoRR = <:cppmem<
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

let test_CoRR_SC = litmus.test_forall
  ~name:"CoRR_SC"
  ~prog:prog_CoRR
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";]
  ~asserto:State.safeo

(* ************************************************************************** *)
(* ********************** ReleaseAcquire Tests ****************************** *)
(* ************************************************************************** *)

let litmus_test_RA ~name ~prog ~regs ~locs ~asserto ~tag =
  let module Interpreter = ConcurrentInterpreter(Operational.SequentialConsistent) in
  let module Trace = Utils.Trace(Interpreter.State) in
  let mem = Operational.ReleaseAcquire.alloc ~thrdn locs in
  let regs = RegStorage.init thrdn @@ Regs.alloc regs in
  let initstate = Interpreter.State.init regs mem in
  litmus_test
    ~pprint:Trace.trace
    ~interpo ~initstate ~name ~prog ~asserto ~tag

let safeo_RA s =
  let module Interpreter = ConcurrentInterpreter(Operational.SequentialConsistent) in
  Interpreter.State.erroro s ~sg:(fun _ -> failure) ~fg:(success)

let dataraceo_RA s =
  let module Interpreter = ConcurrentInterpreter(Operational.SequentialConsistent) in
  Interpreter.State.erroro s ~sg:(fun err -> fresh (mo loc) (err === Error.datarace mo loc))

let prog_SW = <:cppmem<
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

let test_SW_RA = litmus_test_RA
  ~name:"SW_RA"
  ~prog:prog_SW
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~tag:Forall
  ~asserto:safeo_RA

let prog_SB = <:cppmem<
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

let test_SB_RA = litmus_test_RA
  ~name:"SB_RA"
  ~prog:prog_SB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~tag:Exists
  ~asserto:safeo_RA

let prog_LB = <:cppmem<
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

let test_LB_RA = litmus_test_RA
  ~name:"LB+rel+acq_RA"
  ~prog:prog_LB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y";]
  ~tag:Forall
  ~asserto:safeo_RA

let prog_LB_acq_rlx = <:cppmem<
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

let test_LB_acq_rlx_RA = litmus_test_RA
  ~name:"LB+acq+rlx_RA"
  ~prog:prog_LB_acq_rlx
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"]
  ~tag:Forall
  ~asserto:safeo_RA

let prog_MP = <:cppmem<
    spw {{{
        x_rlx := 1;
        f_rel := 1
    |||
        repeat r1 := f_acq until r1;
        r2 := x_rlx;
        return (r2)
    }}};
    assert (r2 = 1)
>>

let test_MP_RA = litmus_test_RA
  ~name:"MP_RA"
  ~prog:prog_MP
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~tag:Forall
  ~asserto:safeo_RA

let prog_MP_rlx_acq = <:cppmem<
    spw {{{
        x_rlx := 1;
        f_rlx := 1
    |||
        repeat r1 := f_acq until r1;
        r2 := x_rlx;
        return (r2)
    }}};
    assert (r2 = 0)
>>

let test_MP_rlx_acq_RA = litmus_test_RA
  ~name:"MP+rlx+acq_RA"
  ~prog:prog_MP_rlx_acq
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~tag:Exists
  ~asserto:safeo_RA

let prog_MP_rel_rlx = <:cppmem<
    spw {{{
        x_rlx := 1;
        f_rel := 1
    |||
        repeat r1 := f_rlx until r1;
        r2 := x_rlx;
        return (r2)
    }}};
    assert (r2 = 0)
>>

let test_MP_rel_rlx_RA = litmus_test_RA
  ~name:"MP+rel+rlx_RA"
  ~prog:prog_MP_rel_rlx
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"; "f"]
  ~tag:Exists
  ~asserto:safeo_RA

let prog_MP_relseq = <:cppmem<
  spw {{{
      x_rlx := 1;
      f_rel := 1;
      f_rlx := 2
  |||
      repeat r1 := f_acq until (r1 = 2);
      r2 := x_rlx;
      return (r2)
  }}};
  assert (r2 = 1)
>>

let test_MP_relseq_RA = litmus_test_RA
  ~name:"MP+relseq_RA"
  ~prog:prog_MP_relseq
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~tag:Forall
  ~asserto:safeo_RA

let prog_CoRR = <:cppmem<
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

let test_CoRR_RA = litmus_test_RA
  ~name:"CoRR_RA"
  ~prog:prog_CoRR
  ~regs:["r1"; "r2"; "r3"; "r4"]
  ~locs:["x";]
  ~tag:Forall
  ~asserto:safeo_RA

let prog_DR1 = <:cppmem<
  spw {{{
      x_rlx := 1
  |||
      r1 := x_na;
      assert (r1 = 0)
  }}}
>>

let test_DR1_RA = litmus_test_RA
  ~name:"DR1_RA"
  ~prog:prog_DR1
  ~regs:["r1"]
  ~locs:["x"]
  ~tag:Exists
  ~asserto:dataraceo_RA

let prog_DR2 = <:cppmem<
  spw {{{
      x_na := 1
  |||
      r1 := x_rlx;
      assert (r1 = 0)
  }}}
>>

let test_DR2_RA = litmus_test_RA
  ~name:"DR2_RA"
  ~prog:prog_DR2
  ~regs:["r1"]
  ~locs:["x"]
  ~tag:Exists
  ~asserto:dataraceo_RA

(*
let prog_LB = <:cppmem<
  spw {{{
    r1 := x_rlx;
    y_rlx := 1;
    ret r1
  |||
    r2 := y_rlx;
    x_rlx := r2
  }}}
>>

let test_LB step = test_prog step prog_LB ["0"; "1"]

let prog_LBd = <:cppmem<
  spw {{{
    r1 := x_rlx;
    y_rlx := r1;
    ret r1
  |||
    r2 := y_rlx;
    x_rlx := r2
  }}}
>>

let test_LBd step = test_prog step prog_LBd ["0";]

let rlx_rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.NonAtomic.all @ Rules.Rlx.all
let rlx_relAcq_rules = rlx_rules @ Rules.RelAcq.all
let sc_rules = rlx_rules @ Rules.SC.all

let rlxStep = Rules.make_reduction_relation (rlx_rules)

let relAcqStep = Rules.make_reduction_relation (rlx_relAcq_rules)

let scStep = Rules.make_reduction_relation (sc_rules)

let promisingStep =
  let module CertStep = (val Rules.Promising.make_certified_step (Rules.Basic.all @ Rules.Rlx.all)) in
  (module struct
    include Semantics.UnionRelation(Rules.ThreadSpawning.Step)(CertStep)
  end : Rules.CppMemStep)
*)

let tests = Test.(
  make_testsuite ~name:"Litmus" ~tests: [

    make_testsuite ~name:"SeqCst" ~tests: [
      test_SW_SC;
      test_SB_SC;
      test_LB_SC;
      test_MP_SC;
      (* test_CoRR_SC; *)
    ];

    make_testsuite ~name:"RelAcq" ~tests: [
      test_SW_RA;
      test_SB_RA;
      test_LB_RA;
      test_LB_acq_rlx_RA;
      test_MP_RA;
      test_MP_rlx_acq_RA;
      test_MP_rel_rlx_RA;
      test_MP_relseq_RA;
      (* test_CoRR_RA; *)
      test_DR1_RA;
      test_DR2_RA;
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
