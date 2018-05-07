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

let prog_DekkerLock_sc = <:cppmem_par<
  spw {{{
      x_sc := 1;
      r1 := y_sc;
      while (r1) do
        r2 := turn_sc;
        if (r2 != 0) then
          x_sc := 0;
          repeat
            r2 := turn_sc
          until (r2 = 0);
          x_sc := 1
        else
          skip
        fi;
        r1 := y_sc
      od;
      (* start of critical section *)
      r3 := v_sc;
      v_sc := (r3 + 1);
      (* end of critical section *)
      turn_sc := 1;
      x_sc := 0
  |||
      y_sc := 1;
      r1 := x_sc;
      while (r1) do
        r2 := turn_sc;
        if (r2 != 1) then
          y_sc := 0;
          repeat
            r2 := turn_sc
          until (r2 = 1);
          y_sc := 1
        else
          skip
        fi;
        r1 := x_sc
      od;
      (* start of critical section *)
      r3 := v_sc;
      v_sc := (r3 + 1);
      (* end of critical section *)
      turn_sc := 0;
      y_sc := 0
  }}}
>>

let prog_DekkerLock_rlx = <:cppmem_par<
  spw {{{
      x_rlx := 1;
      r1 := y_rlx;
      while (r1) do
        r2 := turn_rlx;
        if (r2 != 0) then
          x_rlx := 0;
          repeat
            r2 := turn_rlx
          until (r2 = 0);
          x_rlx := 1
        else
          skip
        fi;
        r1 := y_rlx
      od;
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      turn_rlx := 1;
      x_rlx := 0
  |||
      y_rlx := 1;
      r1 := x_rlx;
      while (r1) do
        r2 := turn_rlx;
        if (r2 != 1) then
          y_rlx := 0;
          repeat
            r2 := turn_rlx
          until (r2 = 1);
          y_rlx := 1
        else
          skip
        fi;
        r1 := x_rlx
      od;
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      turn_rlx := 0;
      y_rlx := 0
  }}}
>>

let prog_DekkerLock_tpl = <:cppmem_par<
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

let test_verify_sc = Test.(make_test_desc
  ~name:"Verify"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Safe
  ~len:Long
  ~n:1
  prog_DekkerLock_sc
)

let test_sc = Test.(make_operational_testsuite ~model:SeqCst [
  test_verify_sc
])

let test_verify_rlx_tso = Test.(make_test_desc
  ~name:"Verify+rlx"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Unsafe
  ~len:Long
  ~n:1
  prog_DekkerLock_rlx
)

let test_synth_tso = Test.(make_test_desc
  ~name:"Synth"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Synth
  ~len:Huge
  ~n:1
  prog_DekkerLock_tpl
)

let test_tso = Test.(make_operational_testsuite ~model:TSO [
  test_verify_rlx_tso;
  (* test_synth_tso; *)
])

let test_verify_rlx_ra = Test.(make_test_desc
  ~name:"Verify+rlx"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Unsafe
  ~len:Long
  ~n:1
  prog_DekkerLock_rlx
)

let test_synth_ra = Test.(make_test_desc
  ~name:"Synth"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Synth
  ~len:Huge
  ~n:1
  prog_DekkerLock_tpl
)

let test_ra = Test.(make_operational_testsuite ~model:RelAcq [
  test_verify_rlx_ra;
  (* test_synth_ra; *)
])

let tests = Test.(make_testsuite ~name:"DekkerLock" [
  test_sc;
  test_tso;
  test_ra;
])
