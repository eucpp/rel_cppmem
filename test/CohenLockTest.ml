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

let prog_CohenLock = <:cppmem_par<
  spw {{{
      r1 := choice(1, 2);
      x_rel := r1;
      repeat r2 := y_acq until (r2);
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
    repeat r2 := x_acq until (r2);
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

(* let () =
  let module Memory = Operational.SeqCst in
  let module Interpreter = Operational.Interpreter(Memory) in
  let module Trace = Utils.Trace(Interpreter.State) in
  let istate = Interpreter.State.alloc_istate ~regs:["r1"; "r2"; "r3"] ~locs:["x"; "y"; "d"] prog_CohenLock in
  Stream.iter ((fun out -> Format.printf "%a@;" Trace.trace out)) @@ Interpreter.eval istate
*)

let test_CohenLock ~stat = Test.(make_test_desc
  ~name:"CohenLock"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "d"]
  ~prop:Prop.(loc "d" = 1)
  ~stat
  prog_CohenLock
)

let tests = Test.(make_testsuite ~name:"Cohen"
  ~tests:[
    (* make_operational_testsuite
      ~model:SeqCst
      ~tests:[ test_CohenLock ~stat:Fulfills ]

    ; *)

    (* make_operational_testsuite
      ~model:TSO
      ~tests:[ test_CohenLock ~stat:Fulfills ]

    ; *)

    make_operational_testsuite
      ~model:RelAcq
      ~tests:[ test_CohenLock ~stat:Fulfills ]
  ]
)

(* let tests = Test.(make_testsuite ~name:"Cohen" ~tests:[]) *)
