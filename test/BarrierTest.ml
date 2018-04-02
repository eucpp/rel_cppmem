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
    x_na := 1;
    (* barrier start *)
    repeat
      r1 := cnt_rlx;
      r2 := CAS(relAcq, rlx, cnt, r1, (r1 - 1))
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
      r2 := CAS(relAcq, rlx, cnt, r1, (r1 - 1))
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

module Interpreter = Operational.Interpreter(Operational.RelAcq)
module Trace = Utils.Trace(Interpreter.State)

let test_Barrier ~stat = Test.(make_test_desc
  ~name:"Barrier"
  ~regs:["r1"; "r2"; "r3"]
  ~mem:[("x", 0); ("y", 0); ("g", 0); ("cnt", 2)]
  ~prop:Prop.((1%"r3" = 1) && (2%"r3" = 1))
  ~stat
  prog_Barrier
)

let tests = Test.(make_testsuite ~name:"Barrier"
  ~tests:[
    (* make_operational_testsuite
      ~model:SeqCst
      ~tests:[ test_Barrier ~stat:Fulfills ]

    ; *)

    (* make_operational_testsuite
      ~model:TSO
      ~tests:[ test_Barrier ~stat:Fulfills ]

    ; *)

    make_operational_testsuite
      ~model:RelAcq
      ~tests:[ test_Barrier ~stat:Fulfills ]
  ]
)
