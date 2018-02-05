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

type ('at, 'bt, 'al, 'bl) assertion =
  ('at, 'al) Semantics.Input.ti -> ('bt, 'bl) Semantics.Output.ti -> MiniKanren.goal

val exec :
  ('at, 'bt, 'ct, 'al, 'bl, 'cl) Semantics.interpreter ->
  ('at, 'al) Semantics.Prog.ti -> ('bt, 'bl) Semantics.Input.ti ->
  ('ct, 'cl) Semantics.Output.reified MiniKanren.Stream.t

val angelic :
  ('at, 'bt, 'ct, 'al, 'bl, 'cl) Semantics.interpreter ->
  ('bt, 'bl) Semantics.tpred -> ('ct, 'cl) Semantics.tpred ->
  ('at, 'al) Semantics.Prog.ti ->
  (('bt, 'bl) Semantics.Input.reified * ('ct, 'cl) Semantics.Input.reified) MiniKanren.Stream.t

val verify :
  interpo:('at, 'bt, 'ct, 'al, 'bl, 'cl) Semantics.interpreter ->
  asserto:('bt, 'ct, 'bl, 'cl) assertion ->
  ('bt, 'bl) Semantics.tpred ->
  ('at, 'al) Semantics.Prog.ti ->
  (('bt, 'bl) Semantics.Input.reified * ('ct, 'cl) Semantics.Input.reified) MiniKanren.Stream.t

val synth :
  ?positive: (('bt, 'bl) MiniKanren.injected -> ('ct, 'cl) MiniKanren.injected -> MiniKanren.goal) list ->
  ?negative: (('bt, 'bl) MiniKanren.injected -> ('ct, 'cl) MiniKanren.injected -> MiniKanren.goal) list ->
  ('at, 'bt, 'ct, 'al, 'bl, 'cl) Semantics.interpreter ->
  ('at, 'al) Semantics.tpred ->
  ('at, 'al) Semantics.Prog.reified MiniKanren.Stream.t
