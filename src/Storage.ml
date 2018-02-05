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
open MiniKanren.Std

type ('at, 'bt) tt = (('at * 'bt), ('at, 'bt) tt) MiniKanren.Std.list

type ('al, 'bl) tl = ('al, 'bl) inner MiniKanren.logic
  and ('al, 'bl) inner = (('al, 'bl) MiniKanren.Std.Pair.logic, ('al, 'bl) tl) MiniKanren.Std.list

type ('at, 'bt, 'al, 'bl) ti = (('at, 'bt) tt, ('al, 'bl) tl) MiniKanren.injected

type ('at, 'al) key = ('at, 'al) MiniKanren.injected
type ('bt, 'bl) value = ('bt, 'bl) MiniKanren.injected

let empty = nil

let allocate default vars = MiniKanren.Std.List.list @@ List.map (fun var -> pair var default) vars

let from_assoc assoc = MiniKanren.Std.List.list @@ List.map (fun (k, v) -> pair k v) assoc

let reify reify_key reify_value = List.reify (Pair.reify reify_key reify_value)

let pprint pp_kv = Utils.pprint_llist (Utils.pprint_logic pp_kv)

(* let rec of_listso ks vs t = conde [
  (ks === nil ()) &&& (vs === nil ()) &&& (t === nil ());

  fresh (k v ks' vs' t')
    (ks === k % ks')
    (vs === v % vs')
    (t  === (pair k v) % t')
    (of_listo ks' vs' t');
] *)

let rec geto vars var value =
  fresh (hd tl)
    (vars === hd % tl)
    (conde [
      (hd === pair var value);
      (*(hd =/= pair var value) &&&*) (geto tl var value);
    ])

let rec seto vars vars' var value =
  fresh (hd tl tl' k v)
    (vars === hd % tl)
    (hd === pair k v)
    (conde [
      (k === var) &&& (vars' === (pair var value) % tl);
      (k =/= var) &&& (vars' === hd % tl') &&& (seto tl tl' var value);
    ])

let keyso t keys = List.mapo (fun kv k -> fresh (v) (kv === pair k v)) t keys

let rec removeo t t' k = conde [
  fresh (hd tl tl' k' v)
    (t === hd % tl)
    (hd === pair k' v)
    (conde [
      (k === k') &&& (t' === tl);
      (k =/= k') &&& (t' === hd % tl') &&& (removeo tl tl' k);
    ])
]

let membero t k v = List.membero t (pair k v)

let extendo t t' k v =
  (* TODO: check that [k] is absent in [t] *)
  (t' === (pair k v) % t)

let rec constro t = function
  | []          -> (t === nil ())
  | (k, p)::xs  ->
    fresh (v tl)
      (t === (pair k v) % tl)
      (p v)
      (constro tl xs)

let shapeo t keys = constro t @@ List.map (fun k -> (k, fun v -> success)) keys

let rec updateo upo t t' var =
  fresh (k v v' tl tl')
    (t  === (pair k v ) % tl )
    (t' === (pair k v') % tl')
    (conde [
      (k === var) &&& (upo v v') &&& (tl === tl');
      (k =/= var) &&& (v === v') &&& (updateo upo tl tl' var);
    ])

let rec mapo relo vars vars' = conde [
  (vars === nil ()) &&& (vars' === nil ());
  (fresh (k v k' v' tl tl')
    (vars  === (Pair.pair k  v ) % tl )
    (vars' === (Pair.pair k' v') % tl')
    (relo k v k' v')
    (mapo relo tl tl'));
  ]

let rec map2o relo vars1 vars2 vars' = conde [
  (vars1 === nil ()) &&& (vars2 === nil ()) &&& (vars' === nil ());
  (fresh (k1 k2 k' v1 v2 v' tl1 tl2 tl')
    (vars1 === (Pair.pair k1 v1) % tl1)
    (vars2 === (Pair.pair k2 v2) % tl2)
    (vars' === (Pair.pair k' v') % tl')
    (relo k1 v1 k2 v2 k' v')
    (map2o relo tl1 tl2 tl'));
  ]

let foldo relo t acc acc' =
  let helper kv acc acc' =
    fresh (k v)
      (kv === pair k v)
      (relo k v acc acc')
  in
  List.foldro helper acc t acc'

let rec forallo relo t = conde [
  (t === nil ());
  fresh (k v t')
    (t === (Pair.pair k v) % t')
    (relo k v)
    (forallo relo t')
]

let checko t kvs = ?& (List.map (fun (k, v) -> geto t k v) kvs)
