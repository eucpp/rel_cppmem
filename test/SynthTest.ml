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

open OUnit2
open MiniKanren
open MiniKanrenStd

open Lang
open Lang.Term
open Lang.Expr
open Lang.Loc
open Lang.Register
open Lang.Value
open MemoryModel

let synth_test ?positive ?negative ?(n=1) ~name ~tplo ~intrpo =
  let progs = Stream.take ~n @@ Query.synth ?positive ?negative intrpo tplo in
  let test () = if List.length progs < n then
    Test.Fail ""
  else
    let module Trace = Utils.Trace(Lang.Term) in
    let i = ref 0 in
    Format.printf "Results@;";
    List.iter (fun t -> i := !i + 1; Format.printf "Synth test %s answer #%d:@;%a@;" name !i Trace.trace t) progs;
    Test.Ok
  in
  Test.make_testcase ~name ~test

let rec expro e = conde [
  fresh (v)
    (e === const v);

  fresh (r)
    (e === var r);

  fresh (op e1 e2)
    (e === binop op e1 e2)
    (expro e1)
    (expro e2);
]

let rec condo e = conde [
  fresh (r)
    (e === var r);

  fresh (op e1 e2)
    (e === binop op e1 e2)
    (expro e1)
    (expro e2);
]

let rec stmto progo t = conde [
  fresh (mo l e v)
    (t === store mo l (const v));

  fresh (mo l r)
    (t === load mo l r);

  fresh (mo x r)
    (t === repeat (load mo x r) (var r));

  fresh (e t1 t2)
    (t === if' e t1 t2)
    (condo e)
    (progo t1)
    (progo t2);

  fresh (e t')
    (t === while' e t')
    (condo e)
    (progo t');

  fresh (e t')
    (t === repeat t' e)
    (condo e)
    (progo t');
]

let rec progo t = conde [
    (stmto progo t);

    fresh (t1 t2)
      (t === seq t1 t2)
      (stmto progo t1)
      (progo t2)
  ]

let mp_sketch = fun h1 h2 -> <:cppmem<
    r1 := x_sc;
    spw {{{
        m_sc := r1;
        ? h1
    |||
        ? h2;
        r2 := m_sc;
        return (r2)
    }}};
    assert (r1 = r2)
>>

let mp_tplo t =
  fresh (h1 h2)
    (t === mp_sketch h1 h2)
    (progo h1)
    (progo h2)

let test_MP_SC =
  let open SequentialConsistent in
  let regs = ["r1"; "r2"] in
  synth_test ~name:"MP_SC" ~n:1 ~intrpo ~tplo:mp_tplo
    ~positive:
    [ (fun i o ->
        fresh (m v)
          (i === State.mem @@ Memory.init ~regs ~mem:[("x", 0); ("f", 0); ("m", 0)])
          (o === State.mem m)
      )
    ; (fun i o ->
        fresh (m)
          (i === State.mem @@ Memory.init ~regs ~mem:[("x", 1); ("f", 0); ("m", 0)])
          (o === State.mem m)
      )
    ]
    ~negative:
      [ (fun i o ->
          fresh (err m)
            (i === State.mem @@ Memory.init ~regs ~mem:[("x", 0); ("f", 0); ("m", 0)])
            (o === State.error err m)
        )
      ; (fun i o ->
          fresh (err m)
            (i === State.mem @@ Memory.init ~regs ~mem:[("x", 1); ("f", 0); ("m", 0)])
            (o === State.error err m)
        )
      ]

let tests = Test.(
  make_testsuite ~name:"Synth" ~tests:[

    make_testsuite ~name:"SeqCst" ~tests:[
      test_MP_SC
    ];

    make_testsuite ~name:"RelAcq" ~tests:[
    ]
  ]
)
