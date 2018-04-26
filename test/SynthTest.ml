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
  ~kind:Synth
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
  ~kind:Synth
  ~n:1
  ~len:Huge
  prog_CohenExcl
)

let prog_Barrier = <:cppmem_par<
  spw {{{
    x_unkw := 1;
    (* barrier start *)
    repeat
      r1 := cnt_unkw;
      r2 := CAS(unkw, unkw, cnt, r1, (r1 - 1))
    until (r1 = r2);
    if (r2 = 1) then
      g_unkw := 1
    else
      repeat
        r1 := g_unkw
      until (r1)
    fi;
    (* barrier end *)
    r3 := y_unkw
  |||
    y_unkw := 1;
    (* barrier start *)
    repeat
      r1 := cnt_unkw;
      r2 := CAS(unkw, unkw, cnt, r1, (r1 - 1))
    until (r1 = r2);
    if (r2 = 1) then
      g_unkw := 1
    else
      repeat
        r1 := g_unkw
      until (r1)
    fi;
    (* barrier end *)
    r3 := x_unkw
  }}}
>>

let test_Barrier = Test.(make_test_desc
  ~name:"Barrier"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "d"]
  ~mem:[("x", 0); ("y", 0); ("g", 0); ("cnt", 2)]
  ~prop:Prop.((1%"r3" = 1) && (2%"r3" = 1))
  ~kind:Synth
  ~len:Huge
  ~n:1
  prog_Barrier
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
  ~kind:Synth
  ~len:Huge
  ~n:1
  prog_DekkerLock
)

let tests_tso = Test.make_operational_testsuite ~model:Test.TSO [
  test_MessagePassing;
  test_CohenExcl;
  test_Barrier;
  (* test_DekkerLock; *)
]

let tests_ra = Test.make_operational_testsuite ~model:Test.RelAcq [
  test_MessagePassing;
  test_CohenExcl;
  test_Barrier;
  (* test_DekkerLock; *)
]

let tests = Test.(make_testsuite ~name:"Synth" [
  tests_tso;
  tests_ra;
])
