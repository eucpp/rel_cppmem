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

let prog_MP h1 h2 = <:cppmem_par<
  spw {{{
    x_na := 1;
    ? h1
  |||
    repeat ? h2 until (r1);
    r2 := x_na
  }}}
>>

module MP(Memory : Operational.MemoryModel) =
  struct
    module Test = Test.OperationalTest(Memory)

    let istateo s =
      let regs = ["r1"; "r2"] in
      let locs = ["x" ; "f" ] in
      fresh (h1 h2 mo1 mo2)
        (s  === Test.make_istate ~regs ~locs @@ prog_MP h1 h2)
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
      (* r2 := CAS(relAcq, rlx, cnt, r1, (r1 - 1)) *)
      ? h3
    until (r1 = r2);
    if (r2 = 1) then
      (* g_rel := 1 *)
      ? h5
    else
      repeat
        (* r1 := g_acq  *)
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
      (* r2 := CAS(relAcq, rlx, cnt, r1, (r1 - 1)) *)
      ? h4
    until (r1 = r2);
    if (r2 = 1) then
      (* g_rel := 1 *)
      ? h6
    else
      repeat
        (* r1 := g_acq  *)
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
      let locs = ["x" ; "y"; "d" ] in
      fresh (h1 h2 h3 h4 h5 h6 h7 h8 mo1 mo2 mo3 mo3' mo4 mo4' mo5 mo6 mo7 mo8)
        (s  === Test.make_istate ~regs ~locs @@ prog_Barrier h1 h2 h3 h4 h5 h6 h7 h8)
        (h1 === store mo1 (loc "cnt") (var @@ reg "r1"))
        (h2 === store mo2 (loc "cnt") (var @@ reg "r1"))
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
        ~name:"Cohen"
        ~prop:Prop.(
          (loc "d" = 1)
        )
        ~n:1
        istateo
  end

module Barrier_TSO = Barrier(Operational.TSO)
module Barrier_SRA = Barrier(Operational.RelAcq)

let barrier_tests = Test.(make_testsuite ~name:"Barrier"
  ~tests:[
    make_testcase ~name:"TSO" ~test:Barrier_TSO.test;
    make_testcase ~name:"SRA" ~test:Barrier_SRA.test;
  ]
)

let tests = Test.(
  make_testsuite ~name:"Synth" ~tests: [
    make_testsuite ~name:"Operational" ~tests: [
      mp_tests;
      cohen_tests;
      barrier_tests;
    ];
  ]
)
