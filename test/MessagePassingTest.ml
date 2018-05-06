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

let prog_MessagePassing_sc = <:cppmem_par<
  spw {{{
    x_sc := 1;
    f_sc := 1
  |||
    repeat
      r1 := f_sc
    until (r1);
    r2 := x_sc
  }}}
>>

let prog_MessagePassing_rel_acq = <:cppmem_par<
  spw {{{
    x_rlx := 1;
    f_rel := 1
  |||
    repeat
      r1 := f_acq
    until (r1);
    r2 := x_rlx
  }}}
>>

let prog_MessagePassing_rlx = <:cppmem_par<
  spw {{{
    x_rlx := 1;
    f_rlx := 1
  |||
    repeat
      r1 := f_rlx
    until (r1);
    r2 := x_rlx
  }}}
>>

let prog_MessagePassing_tpl = <:cppmem_par<
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

let test_verify_sc = Test.(make_test_desc
  ~name:"Verify"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~prop:Prop.(2%"r2" = 1)
  ~kind:Safe
  prog_MessagePassing_sc
)

let test_sc = Test.(make_operational_testsuite ~model:SeqCst [
  test_verify_sc
])

let test_verify_tso = Test.(make_test_desc
  ~name:"Verify"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~prop:Prop.(2%"r2" = 1)
  ~kind:Safe
  prog_MessagePassing_rlx
)

let test_synth_tso = Test.(make_test_desc
  ~name:"Synth"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~prop:Prop.(2%"r2" = 1)
  ~kind:Safe
  prog_MessagePassing_tpl
)

let test_tso = Test.(make_operational_testsuite ~model:TSO [
  test_verify_tso;
  test_synth_tso;
])

let test_verify_rlx_ra = Test.(make_test_desc
  ~name:"Verify+rlx"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~prop:Prop.(2%"r2" = 1)
  ~kind:Unsafe
  prog_MessagePassing_rlx
)

let test_verify_rel_acq_ra = Test.(make_test_desc
  ~name:"Verify+rel+acq"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~prop:Prop.(2%"r2" = 1)
  ~kind:Safe
  prog_MessagePassing_rel_acq
)

let test_synth_ra = Test.(make_test_desc
  ~name:"Synth"
  ~regs:["r1"; "r2"]
  ~locs:["x"; "f"]
  ~prop:Prop.(2%"r2" = 1)
  ~kind:Safe
  ~n:1
  prog_MessagePassing_tpl
)

let test_ra = Test.(make_operational_testsuite ~model:RelAcq [
  test_verify_rlx_ra;
  test_verify_rel_acq_ra;
  test_synth_ra;
])

let tests = Test.(make_testsuite ~name:"MessagePassing" [
  test_sc;
  test_tso;
  test_ra;
])
