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

type ('at, 'bt) tt

type ('al, 'bl) tl = ('al, 'bl) inner MiniKanren.logic
  and ('al, 'bl) inner

type ('at, 'bt, 'al, 'bl) ti = (('at, 'bt) tt, ('al, 'bl) tl) MiniKanren.injected

type ('at, 'bt, 'al, 'bl) ri = (('at, 'bt) tt, ('al, 'bl) tl) MiniKanren.reified

type ('at, 'al) key = ('at, 'al) MiniKanren.injected
type ('bt, 'bl) value = ('bt, 'bl) MiniKanren.injected

val empty : unit -> ('at, 'bt, 'al, 'bl) ti

val allocate : ('bt, 'bl) value -> ('at, 'al) key list -> ('at, 'bt, 'al, 'bl) ti

val from_assoc : (('at, 'al) key * ('bt, 'bl) value) list -> ('at, 'bt, 'al, 'bl) ti

val reify :
  (MiniKanren.helper -> ('at, 'al) MiniKanren.injected -> 'al) ->
  (MiniKanren.helper -> ('bt, 'bl) MiniKanren.injected -> 'bl) ->
  MiniKanren.helper -> ('at, 'bt, 'al, 'bl) ti -> ('al, 'bl) tl

val pprint : (Format.formatter -> 'al * 'bl -> unit) -> Format.formatter -> ('al, 'bl) tl -> unit

val geto : ('at, 'bt, 'al, 'bl) ti ->                            ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal
val seto : ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal

val keyso : ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) MiniKanren.Std.List.groundi -> MiniKanren.goal

val removeo :
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key -> MiniKanren.goal

val membero :
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal

val extendo :
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti ->
  ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal

val updateo :
  (('bt, 'bl) value -> ('bt, 'bl) value -> MiniKanren.goal) ->
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key -> MiniKanren.goal

val mapo :
  (('at, 'al) key -> ('bt, 'bl) value -> ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal) ->
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> MiniKanren.goal

val map2o :
  (('at, 'al) key -> ('bt, 'bl) value -> ('at, 'al) key -> ('bt, 'bl) value -> ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal) ->
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> MiniKanren.goal

val foldo :
  (('at, 'al) key -> ('bt, 'bl) value -> ('acct, _ MiniKanren.logic as 'accl) MiniKanren.injected -> ('acct, 'accl) MiniKanren.injected -> MiniKanren.goal) ->
  ('at, 'bt, 'al, 'bl) ti -> ('acct, 'accl) MiniKanren.injected -> ('acct, 'accl) MiniKanren.injected -> MiniKanren.goal

val forallo :
  (('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal) ->
  ('at, 'bt, 'al, 'bl) ti -> MiniKanren.goal

val shapeo : ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key list -> MiniKanren.goal

val constro :
  ('at, 'bt, 'al, 'bl) ti ->
  (('at, 'al) key * (('bt, 'bl) value -> MiniKanren.goal)) list ->
  MiniKanren.goal

val checko : ('at, 'bt, 'al, 'bl) ti -> (('at, 'al) key * ('bt, 'bl) value) list -> MiniKanren.goal
