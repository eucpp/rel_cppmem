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

open Lang
open Lang.Expr
open Lang.Stmt
open Lang.Loc
open Lang.Reg
open Lang.Value

let prog_MessagePassing = <:cppmem_par<
  spw {{{
    x_rlx := 1;
    f_unkw := 1
  |||
    repeat
      r1 := f_unkw
    until (r1);
    r2 := x_rlx
  }}}
>>

let test_MessagePassing = Test.(make_test_desc
  ~name:"MessagePassing"
  ~regs:["r1"; "r2";]
  ~locs:["x"; "f";]
  ~prop:Prop.(2%"r2" = 1)
  ~stat:Synth
  ~n:1
  prog_MessagePassing
)

let prog_CohenExcl = <:cppmem_par<
  spw {{{
    r1 := choice(1, 2);
    x_unkw := r1;
    repeat
      r2 := y_unkw
    until (r2);
    if (r1 = r2) then
      (* start of critical section *)
      r3 := d_rlx;
      d_rlx := (r3 + 1)
      (* end of critical section *)
    else
      skip
    fi
  |||
    r1 := choice(1, 2);
    y_unkw := r1;
    repeat
      r2 := x_unkw
    until (r2);
    if (r1 != r2) then
      (* start of critical section *)
      r3 := d_rlx;
      d_rlx := (r3 + 1)
      (* end of critical section *)
    else
      skip
    fi
  }}}
>>

let test_CohenExcl = Test.(make_test_desc
  ~name:"CohenExcl"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "d"]
  ~prop:Prop.(loc "d" = 1)
  ~stat:Synth
  ~n:1
  prog_CohenExcl
)

let prog_DekkerLock = <:cppmem_par<
  spw {{{
      x_unkw := 1;
      r1 := y_unkw;
      while (r1) do
        r2 := turn_unkw;
        if (r2 != 0) then
          x_unkw := 0;
          repeat
            r2 := turn_unkw
          until (r2 = 0);
          x_unkw := 1
        else
          skip
        fi;
        r1 := y_unkw
      od;
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      turn_unkw := 1;
      x_unkw := 0
  |||
      y_unkw := 1;
      r1 := x_unkw;
      while (r1) do
        r2 := turn_unkw;
        if (r2 != 1) then
          y_unkw := 0;
          repeat
            r2 := turn_unkw
          until (r2 = 1);
          y_unkw := 1
        else
          skip
        fi;
        r1 := x_unkw
      od;
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      turn_unkw := 0;
      y_unkw := 0
  }}}
>>

let test_DekkerLock = Test.(make_test_desc
  ~name:"DekkerLock"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~stat:Synth
  ~n:1
  prog_DekkerLock
)

let tests_tso = Test.make_operational_testsuite
  ~model:Test.TSO
  ~tests:([
    test_MessagePassing;
    test_CohenExcl;
  ])

let tests_ra = Test.make_operational_testsuite
  ~model:Test.RelAcq
  ~tests:([
    test_MessagePassing;
    test_CohenExcl;
    (* test_DekkerLock; *)
  ])

let tests = Test.(make_testsuite ~name:"Synth"
  ~tests:[
    tests_tso;
    tests_ra;
  ]
)

(*

module MP(Memory : Operational.MemoryModel) =
  struct
    module Test = Test.OperationalTest(Memory)

    let istateo s =
      let regs = ["r1"; "r2"] in
      let locs = ["x" ; "f" ] in
      fresh (h1 h2 mo1 mo2)
        (s === Test.make_istate ~regs ~locs @@ prog_MP h1 h2)
        (* (mo1 === !!MemOrder.RLX) *)
        (* (mo2 === !!MemOrder.RLX) *)
        (h1 === store mo1 (loc "f") (const @@ integer 1))
        (h2 === load mo2 (loc "f") (reg "r1"))

    let test () =
      Test.test_synth
        ~name:"MP"
        ~prop:Prop.(
          (2%"r2" = 1)
        )
        ~n:1
        istateo

  end

module MP_TSO = MP(Operational.TSO)
module MP_SRA = MP(Operational.RelAcq)

let mp_tests = Test.(make_testsuite ~name:"MP"
  ~tests:[
    make_testcase ~name:"TSO" ~test:MP_TSO.test;
    make_testcase ~name:"SRA" ~test:MP_SRA.test;
  ]
)

let prog_DekkerLock
  h1  h2  h3  h4  h5  h6  h7  h8  h9
  h10 h11 h12 h13 h14 h15 h16 h17 h18 =
<:cppmem_par<
  spw {{{
      (* x_sc := 1; *)
      ? h1;
      (* r1 := y_sc; *)
      ? h3;
      while (r1) do
        (* r2 := turn_sc; *)
        ? h5;
        if (r2 != 0) then
          (* x_sc := 0; *)
          ? h7;
          repeat
            (* r2 := turn_sc  *)
            ? h9
          until (r2 = 0);
          (* x_sc := 1 *)
          ? h11
        else
          skip
        fi;
        (* r1 := y_sc *)
        ? h13
      od;
      (* start of critical section *)
      r3 := v_sc;
      v_sc := (r3 + 1);
      (* end of critical section *)
      (* turn_sc := 1; *)
      ? h15;
      (* x_sc := 0 *)
      ? h17
  |||
      (* y_sc := 1; *)
      ? h2;
      (* r1 := x_sc; *)
      ? h4;
      while (r1) do
        (* r2 := turn_sc; *)
        ? h6;
        if (r2 != 1) then
          (* y_sc := 0; *)
          ? h8;
          repeat
            (* r2 := turn_sc  *)
            ? h10
          until (r2 = 1);
          (* y_sc := 1 *)
          ? h12
        else
          skip
        fi;
        (* r1 := x_sc *)
        ? h14
      od;
      (* start of critical section *)
      r3 := v_sc;
      v_sc := (r3 + 1);
      (* end of critical section *)
      (* turn_sc := 0; *)
      ? h16;
      (* y_sc := 0 *)
      ? h18
  }}}
>>

module Dekker(Memory : Operational.MemoryModel) =
  struct
    module Test = Test.OperationalTest(Memory)

    let istateo s =
      let regs = ["r1"; "r2"; "r3"] in
      let locs = ["x"; "y"; "turn"; "v"] in
      fresh (
        h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12 h13 h14 h15 h16 h17 h18
        mo1 mo2 mo3 mo4 mo5 mo6 mo7 mo8 mo9 mo10 mo11 mo12 mo13 mo14 mo15 mo16 mo17 mo18
      )
        (s  === Test.make_istate ~regs ~locs @@
          prog_DekkerLock h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12 h13 h14 h15 h16 h17 h18
        )
        (h1   === store mo1  (loc "x") (const @@ integer 1))
        (h2   === store mo2  (loc "y") (const @@ integer 1))
        (h3   === load  mo3  (loc "y") (reg "r1"))
        (h4   === load  mo4  (loc "x") (reg "r1"))
        (h5   === load  mo5  (loc "turn") (reg "r2"))
        (h6   === load  mo6  (loc "turn") (reg "r2"))
        (h7   === store mo7  (loc "x") (const @@ integer 0))
        (h8   === store mo8  (loc "y") (const @@ integer 0))
        (h9   === store mo9  (loc "turn") (var @@ reg "r2"))
        (h10  === store mo10 (loc "turn") (var @@ reg "r2"))
        (h11  === store mo11 (loc "x") (const @@ integer 1))
        (h12  === store mo12 (loc "y") (const @@ integer 1))
        (h13  === load  mo13 (loc "y") (reg "r1"))
        (h14  === load  mo14 (loc "x") (reg "r1"))
        (h15  === store mo15 (loc "turn") (const @@ integer 1))
        (h16  === store mo16 (loc "turn") (const @@ integer 0))
        (h17  === store mo17 (loc "x") (const @@ integer 0))
        (h18  === store mo18 (loc "y") (const @@ integer 0))

    let test () =
      Test.test_synth
        ~name:"Dekker"
        ~prop:Prop.(loc "v" = 2)
        ~n:1
        istateo
  end

module Dekker_TSO = Dekker(Operational.TSO)
module Dekker_SRA = Dekker(Operational.RelAcq)

let dekker_tests = Test.(make_testsuite ~name:"Dekker"
  ~tests:[
    make_testcase ~name:"TSO" ~test:Dekker_TSO.test;
    (* make_testcase ~name:"SRA" ~test:Dekker_SRA.test; *)
  ]
)

let prog_CohenLock h1 h2 h3 h4 = <:cppmem_par<
  spw {{{
      r1 := choice(1, 2);
      (* x_rel := r1;  *)
      ? h1;
      (* repeat r2 := y_acq until (r2); *)
      ? h3;
      if (r1 = r2) then
        (* start of critical section *)
        r3 := d_na;
        d_na := (r3 + 1)
        (* end of critical section *)
      else
        skip
      fi
  |||
    r1 := choice(1, 2);
    (* y_rel := r1; *)
    ? h2;
    (* repeat r2 := x_acq until (r2); *)
    ? h4;
    if (r1 != r2) then
      (* start of critical section *)
      r3 := d_na;
      d_na := (r3 + 1)
      (* end of critical section *)
    else
      skip
    fi
  }}}
>>

module CohenLock(Memory : Operational.MemoryModel) =
  struct
    module Test = Test.OperationalTest(Memory)

    let istateo s =
      let regs = ["r1"; "r2"; "r3"] in
      let locs = ["x" ; "y"; "d" ] in
      fresh (h1 h2 h3 h4 mo1 mo2 mo3 mo4)
        (s  === Test.make_istate ~regs ~locs @@ prog_CohenLock h1 h2 h3 h4)
        (h1 === store mo1 (loc "x") (var @@ reg "r1"))
        (h2 === store mo2 (loc "y") (var @@ reg "r1"))
        (h3 === repeat (single @@ load mo3 (loc "y") (reg "r2")) (var @@ reg "r2"))
        (h4 === repeat (single @@ load mo4 (loc "x") (reg "r2")) (var @@ reg "r2"))

    let test () =
      Test.test_synth
        ~name:"Cohen"
        ~prop:Prop.(
          (loc "d" = 1)
        )
        ~n:1
        istateo
  end

module Cohen_TSO = CohenLock(Operational.TSO)
module Cohen_SRA = CohenLock(Operational.RelAcq)

let cohen_tests = Test.(make_testsuite ~name:"Cohen"
  ~tests:[
    make_testcase ~name:"TSO" ~test:Cohen_TSO.test;
    make_testcase ~name:"SRA" ~test:Cohen_SRA.test;
  ]
)

let prog_Barrier h1 h2 h3 h4 h5 h6 h7 h8 = <:cppmem_par<
  spw {{{
    x_na := 1;
    (* barrier start *)
    repeat
      (* r1 := cnt_rlx; *)
      ? h1;
      (* r2 := CAS(relAcq, relAcq, cnt, r1, (r1 - 1)) *)
      ? h3
    until (r1 = r2);
    if (r2 = 1) then
      (* g_rel := 1 *)
      ? h5
    else
      repeat
        (* r1 := g_acq *)
        ? h7
      until (r1)
    fi;
    (* barrier end *)
    r3 := y_na
  |||
    y_na := 1;
    (* barrier start *)
    repeat
      (* r1 := cnt_rlx; *)
      ? h2;
      (* r2 := CAS(relAcq, relAcq, cnt, r1, (r1 - 1)) *)
      ? h4
    until (r1 = r2);
    if (r2 = 1) then
      (* g_rel := 1 *)
      ? h6
    else
      repeat
        (* r1 := g_acq *)
        ? h8
      until r1
    fi;
    (* barrier end *)
    r3 := x_na
  }}}
>>

module Barrier(Memory : Operational.MemoryModel) =
  struct
    module Test = Test.OperationalTest(Memory)

    let istateo s =
      let regs = ["r1"; "r2"; "r3"] in
      let mem = [("x", 0); ("y", 0); ("g", 0); ("cnt", 2)] in
      fresh (h1 h2 h3 h4 h5 h6 h7 h8 mo1 mo2 mo3 mo3' mo4 mo4' mo5 mo6 mo7 mo8)
        (s  === Test.make_istate ~regs ~mem @@ prog_Barrier h1 h2 h3 h4 h5 h6 h7 h8)
        (h1 === load mo1 (loc "cnt") (reg "r1"))
        (h2 === load mo2 (loc "cnt") (reg "r1"))
        (h3 === cas
          mo3 mo3'
          (loc "cnt")
          (var @@ reg "r1")
          (binop (Bop.bop "-") (var @@ reg "r1") (const @@ integer 1))
          (reg "r2")
        )
        (h4 === cas
          mo4 mo4'
          (loc "cnt")
          (var @@ reg "r1")
          (binop (Bop.bop "-") (var @@ reg "r1") (const @@ integer 1))
          (reg "r2")
        )
        (h5 === store mo5 (loc "g") (const @@ integer 1))
        (h6 === store mo6 (loc "g") (const @@ integer 1))
        (h7 === load mo7 (loc "g") (reg "r1"))
        (h8 === load mo8 (loc "g") (reg "r1"))

    let test () =
      Test.test_synth
        ~name:"Barrier"
        ~prop:Prop.((1%"r3" = 1) && (2%"r3" = 1))
        ~n:1
        istateo
  end

module Barrier_TSO = Barrier(Operational.TSO)
module Barrier_SRA = Barrier(Operational.RelAcq)

let barrier_tests = Test.(make_testsuite ~name:"Barrier"
  ~tests:[
    (* make_testcase ~name:"TSO" ~test:Barrier_TSO.test; *)
    make_testcase ~name:"SRA" ~test:Barrier_SRA.test;
  ]
)

let tests = Test.(
  make_testsuite ~name:"Synth" ~tests: [
    make_testsuite ~name:"Operational" ~tests: [
      (* mp_tests; *)
      (* cohen_tests; *)
      (* barrier_tests; *)
      dekker_tests
    ];
  ]
)

*)
