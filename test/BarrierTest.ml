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

let prog_Barrier = <:cppmem_par<
  spw {{{
    x_sc := 1;
    (* barrier start *)
    repeat
      r1 := cnt_sc;
      r2 := CAS(sc, sc, cnt, r1, (r1 - 1))
    until (r1 = r2);
    if (r2 = 1) then
      g_sc := 1
    else
      repeat
        r1 := g_sc
      until (r1)
    fi;
    (* barrier end *)
    r3 := y_sc
  |||
    y_sc := 1;
    (* barrier start *)
    repeat
      r1 := cnt_sc;
      r2 := CAS(sc, sc, cnt, r1, (r1 - 1))
    until (r1 = r2);
    if (r2 = 1) then
      g_sc := 1
    else
      repeat
        r1 := g_sc
      until (r1)
    fi;
    (* barrier end *)
    r3 := x_sc
  }}}
>>

let test_sc = Test.(make_operational_testsuite ~model:SeqCst [
  make_test_desc
    ~name:"Barrier"
    ~regs:["r1"; "r2"; "r3"]
    ~mem:[("x", 0); ("y", 0); ("g", 0); ("cnt", 2)]
    ~prop:Prop.((1%"r3" = 1) && (2%"r3" = 1))
    ~kind:Safe
    ~len:Long
    prog_Barrier
  ])

let prog_Barrier = <:cppmem_par<
  spw {{{
    x_rlx := 1;
    (* barrier start *)
    repeat
      r1 := cnt_rlx;
      r2 := CAS(sc, sc, cnt, r1, (r1 - 1))
    until (r1 = r2);
    if (r2 = 1) then
      g_rlx := 1
    else
      repeat
        r1 := g_rlx
      until (r1)
    fi;
    (* barrier end *)
    r3 := y_rlx
  |||
    y_rlx := 1;
    (* barrier start *)
    repeat
      r1 := cnt_rlx;
      r2 := CAS(sc, sc, cnt, r1, (r1 - 1))
    until (r1 = r2);
    if (r2 = 1) then
      g_rlx := 1
    else
      repeat
        r1 := g_rlx
      until (r1)
    fi;
    (* barrier end *)
    r3 := x_rlx
  }}}
>>

let test_tso = Test.(make_operational_testsuite ~model:TSO [
    make_test_desc
      ~name:"Barrier"
      ~regs:["r1"; "r2"; "r3"]
      ~mem:[("x", 0); ("y", 0); ("g", 0); ("cnt", 2)]
      ~prop:Prop.((1%"r3" = 1) && (2%"r3" = 1))
      ~kind:Safe
      ~len:Long
      prog_Barrier
  ])

let prog_Barrier = <:cppmem_par<
  spw {{{
    x_na := 1;
    (* barrier start *)
    repeat
      r1 := cnt_rlx;
      r2 := CAS(relAcq, relAcq, cnt, r1, (r1 - 1))
    until (r1 = r2);
    if (r2 = 1) then
      g_rel := 1
    else
      repeat r1 := g_acq until r1
    fi;
    (* barrier end *)
    r3 := y_na
  |||
    y_na := 1;
    (* barrier start *)
    repeat
      r1 := cnt_rlx;
      r2 := CAS(relAcq, relAcq, cnt, r1, (r1 - 1))
    until (r1 = r2);
    if (r2 = 1) then
      g_rel := 1
    else
      repeat r1 := g_acq until r1
    fi;
    (* barrier end *)
    r3 := x_na
  }}}
>>

let test_ra = Test.(make_operational_testsuite ~model:RelAcq [
  make_test_desc
    ~name:"Barrier"
    ~regs:["r1"; "r2"; "r3"]
    ~mem:[("x", 0); ("y", 0); ("g", 0); ("cnt", 2)]
    ~prop:Prop.((1%"r3" = 1) && (2%"r3" = 1))
    ~kind:Safe
    ~len:Long
    prog_Barrier
])

let tests = Test.(make_testsuite ~name:"VerifyBarrier" [
  test_sc;
  test_tso;
  test_ra;
])
