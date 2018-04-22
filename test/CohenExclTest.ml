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

let prog_CohenExcl = <:cppmem_par<
  spw {{{
    r1 := choice(1, 2);
    x_sc := r1;
    repeat
      r2 := y_sc
    until (r2);
    if (r1 = r2) then
      (* start of critical section *)
      r3 := d_sc;
      d_sc := (r3 + 1)
      (* end of critical section *)
    else
      skip
    fi
  |||
    r1 := choice(1, 2);
    y_sc := r1;
    repeat
      r2 := x_sc
    until (r2);
    if (r1 != r2) then
      (* start of critical section *)
      r3 := d_sc;
      d_sc := (r3 + 1)
      (* end of critical section *)
    else
      skip
    fi
  }}}
>>

let test_sc = Test.(make_operational_testsuite ~model:SeqCst [
    make_test_desc
      ~name:"CohenExcl"
      ~regs:["r1"; "r2"; "r3"]
      ~locs:["x"; "y"; "d"]
      ~prop:Prop.(loc "d" = 1)
      ~stat:Fulfills
      ~len:Long
      prog_CohenExcl
])

let prog_CohenExcl = <:cppmem_par<
  spw {{{
    r1 := choice(1, 2);
    x_rlx := r1;
    repeat
      r2 := y_rlx
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
    y_rlx := r1;
    repeat
      r2 := x_rlx
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

let test_tso = Test.(make_operational_testsuite ~model:TSO [
  make_test_desc
    ~name:"CohenExcl"
    ~regs:["r1"; "r2"; "r3"]
    ~locs:["x"; "y"; "d"]
    ~prop:Prop.(loc "d" = 1)
    ~stat:Fulfills
    ~len:Long
    prog_CohenExcl
])

let prog_CohenExcl = <:cppmem_par<
  spw {{{
    r1 := choice(1, 2);
    x_rel := r1;
    repeat
      r2 := y_acq
    until (r2);
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
    y_rel := r1;
    repeat
      r2 := x_acq
    until (r2);
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

let test_ra = Test.(make_operational_testsuite ~model:RelAcq [
  make_test_desc
    ~name:"CohenExcl"
    ~regs:["r1"; "r2"; "r3"]
    ~locs:["x"; "y"; "d"]
    ~prop:Prop.(loc "d" = 1)
    ~stat:Fulfills
    ~len:Long
    prog_CohenExcl
])

let tests = Test.(make_testsuite ~name:"VerifyCohenExcl" [
  test_sc;
  test_tso;
  test_ra;
])
