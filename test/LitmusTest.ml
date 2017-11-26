open OUnit2
open MiniKanren
open MiniKanrenStd

open Lang
open Lang.Term
open Lang.Expr
open Lang.Loc
open Lang.Register
open Lang.Value
open MemoryModel

type litmus_test_tag = Exists | Forall

let litmus_test ?trace ~tag evalo asserto prog : test_fun = fun test_ctx ->
  (* litmus tests do not take any input *)
  let inputo _ = MiniKanren.success in
  (* for litmus test [intrpo] just evaluates the program *)
  let intrpo prog _ output = evalo prog output in
  (* for litmus test [asserto] checks final state of abstract machine *)
  let asserto _ output = asserto output in
  let ff = Format.std_formatter in
  match tag with
  | Exists -> (
    let stream = Query.verify_exists intrpo inputo asserto prog in
    if not @@ Stream.is_empty stream then
      ()
    else (
      Format.fprintf ff "Verification query fails!@;";
      assert_failure @@ ""
  ))
  | Forall -> (
    let stream = Query.verify intrpo inputo asserto prog in
    if Stream.is_empty stream then
      ()
    else
      (Format.fprintf ff "Verification query fails!@;";
      begin match trace with
        | Some trace ->
          let cexs = Stream.take stream in
          Format.fprintf ff "List of counterexamples:@;";
          (* again, input is irrelevant for litmus test, we show only final state *)
          List.iter (fun (_, cex) -> Format.fprintf ff "%a@;" trace cex) cexs
        | None -> ()
      end;
      assert_failure @@ "")
    )

let asserto_RA t =
  fresh (p s)
    (t === ReleaseAcquire.Node.cfg p s)
    (p =/= stuck ())

let asserto_DR_RA t =
  fresh (p s)
    (t === ReleaseAcquire.Node.cfg p s)
    (p === stuck ())

let trace_RA =
  let module Trace = Utils.Trace(ReleaseAcquire.Node) in
  Trace.trace

let prog_SW = <:cppmem<
  spw {{{
      x_rlx := 1;
      f_rel := 1
      (* store x_rlx 1; *)
      (* store f_rel 1 *)
  |||
      r1 := f_acq;
      r2 := x_rlx;
      (* load f_acq r1;
      load x_rlx r2; *)
      assert (
        (r1 = 0 && r2 = 0) ||
        (r1 = 0 && r2 = 1) ||
        (r1 = 1 && r2 = 1)
      )
  }}}
>>

(* let _ = Term.show prog_SW *)

let test_SW_RA =
  let state = ReleaseAcquire.State.init ~regs:[reg "r1"; reg "r2"] ~locs:[loc "x"; loc "f"] in
  let node  = ReleaseAcquire.Node.cfg prog_SW state in
  litmus_test ~trace:trace_RA ~tag:Forall ReleaseAcquire.evalo asserto_RA node

let prog_DR1 = <:cppmem<
  spw {{{
      x_rlx := 1
      (* store x_rlx 1 *)
  |||
      r1 := x_na;
      (* load x_na r1; *)
      assert (r1 = 0)
  }}}
>>

let test_DR1_RA =
  let state = ReleaseAcquire.State.init ~regs:[reg "r1";] ~locs:[loc "x"] in
  let node  = ReleaseAcquire.Node.cfg prog_DR1 state in
  litmus_test ~trace:trace_RA ~tag:Exists ReleaseAcquire.evalo asserto_DR_RA node


let prog_DR2 = <:cppmem<
  spw {{{
      x_na := 1
      (* store x_na 1 *)
  |||
      r1 := x_rlx;
      (* load x_rlx r1; *)
      assert (r1 = 0)
  }}}
>>

let test_DR2_RA =
  let state = ReleaseAcquire.State.init ~regs:[reg "r1";] ~locs:[loc "x"] in
  let node  = ReleaseAcquire.Node.cfg prog_DR2 state in
  litmus_test ~trace:trace_RA ~tag:Exists ReleaseAcquire.evalo asserto_DR_RA node

let prog_SB = <:cppmem<
  spw {{{
      x_rel := 1;
      r1 := y_acq;
      a_rlx := r1
      (* store x_rel 1;
      load y_acq r1;
      store a_rlx r1 *)
  |||
      y_rel := 1;
      r2 := x_acq;
      b_rlx := r2
      (* store y_rel 1;
      load x_acq r2;
      store b_rlx r2 *)
  }}}
>>

let test_SB_RA =
  let state = ReleaseAcquire.State.init ~regs:[reg "r1"; reg "r2"] ~locs:[loc "x"; loc "y"; loc "a"; loc "b"] in
  let node  = ReleaseAcquire.Node.cfg prog_SB state in
  let asserto t =
    fresh (p s)
      (t === ReleaseAcquire.Node.cfg p s)
      (ReleaseAcquire.State.checko s (loc "a") (integer 1))
      (ReleaseAcquire.State.checko s (loc "b") (integer 1))
  in
  litmus_test ~trace:trace_RA ~tag:Exists ReleaseAcquire.evalo asserto node


let prog_LB_RA = <:cppmem<
    spw {{{
        r1 := x_acq;
        y_rel := 1;
        a_rlx := r1
    |||
        r2 := y_acq;
        x_rel := 1;
        b_rlx := r2
    }}}
>>

let test_LB_RA =
  let state = ReleaseAcquire.State.init ~regs:[reg "r1"; reg "r2"] ~locs:[loc "x"; loc "y"; loc "a"; loc "b"] in
  let node  = ReleaseAcquire.Node.cfg prog_LB_RA state in
  let asserto t =
    fresh (p s a b)
      (t === ReleaseAcquire.Node.cfg p s)
      (ReleaseAcquire.State.checko s (loc "a") a)
      (ReleaseAcquire.State.checko s (loc "b") b)
      ((a =/= integer 1) ||| (b =/= integer 1))
  in
  litmus_test ~trace:trace_RA ~tag:Forall ReleaseAcquire.evalo asserto node

(*
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

let prog_SB_sc = <:cppmem<
  x_sc := 0;
  y_sc := 0;
  spw {{{
      x_sc := 1;
      r1 := y_sc;
      ret r1
  |||
      y_sc := 1;
      r2 := x_sc;
      ret r2
  }}}
>>

let test_SB_sc step = test_prog step prog_SB_sc ["(1, 0)"; "(0, 1)"; "(1, 1)"]

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

let test_CoRR_rlx step = test_prog step ~negative:true prog_CoRR_rlx ["((1, 2), (2, 1))"; "((2, 1), (1, 2))"]

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

let tests =
  "Litmus">::: [
    "RelAcq">::: [
      "SW">:: test_SW_RA;
      "SB">:: test_SB_RA;
      "LB">:: test_LB_RA;
      "DR1">:: test_DR1_RA;
      "DR2">:: test_DR2_RA;
    ]
    
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
    "LBd">:: test_LBd promisingStep; *)
  ]
