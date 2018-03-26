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

module Injected :
  sig
    type ('tt, 'tl) ti = ('tt, 'tl) MiniKanren.injected
    type ('tt, 'tl) reified = ('tt, 'tl) MiniKanren.reified
  end

module type Logic =
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected
    type ri = (tt, tl) MiniKanren.reified

    val reify : MiniKanren.helper -> ti -> tl

    val pprint : Format.formatter -> tl -> unit
  end

val list_all : (('a, 'b) MiniKanren.injected -> MiniKanren.goal) -> ('a, 'b) MiniKanren.Std.List.groundi -> MiniKanren.goal

val foldlo :
  g:(('a, 'b) MiniKanren.injected -> ('acct, 'accl) MiniKanren.injected -> ('acct, 'accl) MiniKanren.injected -> MiniKanren.goal) ->
  init:('acct, 'accl) MiniKanren.injected ->
  res:('acct, 'accl) MiniKanren.injected ->
  ('a, 'b) MiniKanren.Std.List.groundi -> MiniKanren.goal

val membero : ('a, 'b) MiniKanren.Std.List.groundi -> ('a, 'b) MiniKanren.injected -> MiniKanren.goal

module Trace(T : Logic) :
  sig
    val trace : Format.formatter -> (T.tt, T.tl) MiniKanren.reified -> unit
  end

val pprint_logic : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a MiniKanren.logic -> unit

val pprint_llist : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a MiniKanrenStd.List.logic -> unit

val pprint_nat : Format.formatter -> MiniKanrenStd.Nat.logic -> unit
val pprint_string : Format.formatter -> string MiniKanren.logic -> unit

val zip3 : 'a MiniKanren.Stream.t ->
           'b MiniKanren.Stream.t ->
           'c MiniKanren.Stream.t ->
           ('a * 'b * 'c) MiniKanren.Stream.t

val zip4 : 'a MiniKanren.Stream.t ->
           'b MiniKanren.Stream.t ->
           'c MiniKanren.Stream.t ->
           'd MiniKanren.Stream.t ->
           ('a * 'b * 'c * 'd) MiniKanren.Stream.t

val zip5 : 'a MiniKanren.Stream.t ->
           'b MiniKanren.Stream.t ->
           'c MiniKanren.Stream.t ->
           'd MiniKanren.Stream.t ->
           'e MiniKanren.Stream.t ->
           ('a * 'b * 'c * 'd * 'e) MiniKanren.Stream.t

val zip6 : 'a MiniKanren.Stream.t ->
           'b MiniKanren.Stream.t ->
           'c MiniKanren.Stream.t ->
           'd MiniKanren.Stream.t ->
           'e MiniKanren.Stream.t ->
           'f MiniKanren.Stream.t ->
           ('a * 'b * 'c * 'd * 'e * 'f) MiniKanren.Stream.t


module Option :
  sig
    exception No_value

    val is_some : 'a option -> bool
    val is_none : 'a option -> bool
    val get : 'a option -> 'a
  end
