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

let litmus_test_exists ?pprint ~intrpo ~name ~prog ~initstate ~asserto : test_fun = fun test_ctx ->
  let inputo s = (s === initstate) in
  let stream = Query.angelic intrpo inputo asserto prog in
  if not @@ Stream.is_empty stream then
    ()
  else begin
    let ff = Format.std_formatter in
    Format.fprintf ff "Litmus test %s fails!@;" name;
    begin match pprint with
      | Some pprint ->
        let outs = Query.exec intrpo prog initstate in
        Format.fprintf ff "List of outputs:@;";
        (* input is irrelevant for litmus test, we show only final state *)
        Stream.iter (fun out -> Format.fprintf ff "%a@;" pprint out) outs
      | None -> ()
    end;
    assert_failure @@ ""
  end

let litmus_test_forall ?pprint ~intrpo ~name ~prog ~initstate ~asserto : test_fun = fun test_ctx ->
  let inputo s = (s === initstate) in
  let asserto _ output = asserto output in
  let stream = Query.verify intrpo inputo asserto prog in
  if Stream.is_empty stream then
    ()
  else begin
    let ff = Format.std_formatter in
    Format.fprintf ff "Litmus test %s fails!@;" name;
    begin match pprint with
      | Some pprint ->
        let cexs = Stream.take stream in
        Format.fprintf ff "List of counterexamples:@;";
        (* input is irrelevant for litmus test, we show only final state *)
        List.iter (fun (_, cex) -> Format.fprintf ff "%a@;" pprint cex) cexs
      | None -> ()
    end;
    assert_failure @@ ""
    end

type litmus_test_tag = Exists | Forall

let litmus_test ?pprint ~intrpo ~name ~prog ~initstate ~asserto ~tag =
  match tag with
  | Exists -> litmus_test_exists ?pprint ~intrpo ~name ~prog ~initstate ~asserto
  | Forall -> litmus_test_forall ?pprint ~intrpo ~name ~prog ~initstate ~asserto

let litmus_test_RA ~name ~prog ~regs ~locs ~asserto ~tag =
  let module Trace = Utils.Trace(ReleaseAcquire.State) in
  let mem = List.map (fun l -> (l, 0)) locs in
  let initstate = ReleaseAcquire.State.mem @@ ReleaseAcquire.Memory.init ~regs ~mem in
  litmus_test
    ~pprint:Trace.trace
    ~intrpo:ReleaseAcquire.intrpo
    ~initstate ~name ~prog ~asserto ~tag

let safeo_RA s = ?~(
  fresh (err m)
    (s === ReleaseAcquire.State.error err m)
  )

let dataraceo_RA s =
  fresh (m)
    (s === ReleaseAcquire.State.error !!Error.DataRace m)

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

let prog_SB = <:cppmem<
  spw {{{
      x_rel := 1;
      r1 := y_acq;
      a_rlx := r1
  |||
      y_rel := 1;
      r2 := x_acq;
      b_rlx := r2
  }}}
>>

let test_SB_RA = litmus_test_RA
  ~name:"SB_RA"
  ~prog:prog_SB
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"; "a"; "b"]
  ~tag:Exists
  ~asserto:(fun t ->
    fresh (m)
      (t === ReleaseAcquire.State.mem m)
      (ReleaseAcquire.Memory.checko m (loc "a") (integer 1))
      (ReleaseAcquire.Memory.checko m (loc "b") (integer 1))
  )

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

let test_LB_RA = litmus_test_RA
  ~name:"LB_RA"
  ~prog:prog_LB_RA
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"; "a"; "b"]
  ~tag:Forall
  ~asserto:(fun t ->
    fresh (m a b)
      (t === ReleaseAcquire.State.mem m)
      (ReleaseAcquire.Memory.checko m (loc "a") a)
      (ReleaseAcquire.Memory.checko m (loc "b") b)
      ((a =/= integer 1) ||| (b =/= integer 1))
  )

let prog_LB_rel_acq_rlx = <:cppmem<
    spw {{{
        r1 := x_acq;
        y_rlx := 1;
        a_rlx := r1
    |||
        r2 := y_rlx;
        x_rel := 1;
        b_rlx := r2
    }}}
>>

let test_LB_RA_rlx = litmus_test_RA
  ~name:"LB_RA+rlx"
  ~prog:prog_LB_RA
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"; "a"; "b"]
  ~tag:Forall
  ~asserto:(fun t ->
    fresh (m a b)
      (t === ReleaseAcquire.State.mem m)
      (ReleaseAcquire.Memory.checko m (loc "a") a)
      (ReleaseAcquire.Memory.checko m (loc "b") b)
      ((a =/= integer 1) ||| (b =/= integer 1))
  )

let prog_MP = <:cppmem<
    spw {{{
        x_rlx := 1;
        f_rel := 1
    |||
        repeat r1 := f_acq until r1;
        r2 := x_rlx;
        y_rlx := r2
    }}}
>>

let test_MP_RA = litmus_test_RA
  ~name:"MP_RA"
  ~prog:prog_MP
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"; "f"]
  ~tag:Forall
  ~asserto:(fun t ->
    fresh (m)
      (t === ReleaseAcquire.State.mem m)
      (ReleaseAcquire.Memory.checko m (loc "y") (integer 1))
  )


let prog_MP_rlx1 = <:cppmem<
    spw {{{
        x_rlx := 1;
        f_rlx := 1
    |||
        repeat r1 := f_acq until r1;
        r2 := x_rlx;
        y_rlx := r2
    }}}
>>

let test_MP_RA_rlx1 = litmus_test_RA
  ~name:"MP_RA+rlx1"
  ~prog:prog_MP_rlx1
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"; "f"]
  ~tag:Exists
  ~asserto:(fun t ->
    fresh (m)
      (t === ReleaseAcquire.State.mem m)
      (ReleaseAcquire.Memory.checko m (loc "y") (integer 0))
  )

let prog_MP_rlx_2 = <:cppmem<
    spw {{{
        x_rlx := 1;
        f_rel := 1
    |||
        repeat r1 := f_rlx until r1;
        r2 := x_rlx;
        y_rlx := r2
    }}}
>>

let test_MP_RA_rlx2 = litmus_test_RA
  ~name:"MP_RA+rlx2"
  ~prog:prog_MP_rlx_2
  ~regs:["r1"; "r2"]
  ~locs:["x"; "y"; "f"]
  ~tag:Exists
  ~asserto:(fun t ->
    fresh (m)
      (t === ReleaseAcquire.State.mem m)
      (ReleaseAcquire.Memory.checko m (loc "y") (integer 0))
  )

(*)
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
      "LB+rlx">:: test_LB_RA_rlx;
      "test_MP_RA">:: test_MP_RA;
      "test_MP_RA+rlx1">:: test_MP_RA_rlx1;
      "test_MP_RA+rlx2">:: test_MP_RA_rlx2;
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
