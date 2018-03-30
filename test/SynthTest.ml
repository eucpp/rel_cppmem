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
    ? h2;
    r2 := x_na
  }}}
>>

module RelAcqTest = Test.OperationalTest(Operational.RelAcq)

let istate_MPo s =
  let regs = ["r1"; "r2"] in
  let locs = ["x" ; "f" ] in
  fresh (h1 h2 mo1 mo2)
    (s  === RelAcqTest.make_istate ~regs ~locs @@ prog_MP h1 h2)
    (h1 === store mo1 (loc "f") (const @@ integer 1))
    (h2 === repeat (single @@ load mo2 (loc "f") (reg "r1")) (var @@ reg "r1"))


let test_MP () =
  RelAcqTest.test_synth
    ~name:"MP"
    ~prop:Prop.(
      (2%"r2" = 1)
    )
    ~n:1
    istate_MPo

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

let istate_Coheno s =
  let regs = ["r1"; "r2"; "r3"] in
  let locs = ["x" ; "y"; "d" ] in
  fresh (h1 h2 h3 h4 mo1 mo2 mo3 mo4)
    (s  === RelAcqTest.make_istate ~regs ~locs @@ prog_CohenLock h1 h2 h3 h4)
    (h1 === store mo1 (loc "x") (var @@ reg "r1"))
    (h2 === store mo2 (loc "y") (var @@ reg "r1"))
    (h3 === repeat (single @@ load mo3 (loc "y") (reg "r2")) (var @@ reg "r2"))
    (h4 === repeat (single @@ load mo4 (loc "x") (reg "r2")) (var @@ reg "r2"))

let test_Cohen () =
  RelAcqTest.test_synth
    ~name:"MP"
    ~prop:Prop.(
      (loc "d" = 1)
    )
    ~n:3
    istate_Coheno

let tests_ra_op =
  Test.(make_testsuite ~name:"Synth" ~tests: [
    make_testcase ~name:"MP" ~test:test_MP;
    make_testcase ~name:"Cohen" ~test:test_Cohen;
  ])

let tests = Test.(
  make_testsuite ~name:"Synth" ~tests: [
    make_testsuite ~name:"Operational" ~tests: [
      tests_ra_op;
      (* tests_tso_op; *)
      (* tests_ra_op; *)
    ];


  ]
)
