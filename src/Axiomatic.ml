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

open Lang
open Utils

module EventID =
  struct
    type tt = ThreadID.tt * Std.Nat.ground

    type tl = inner MiniKanren.logic
      and inner = ThreadID.tl * Std.Nat.logic

    type ti = (tt, tl) MiniKanren.injected

    let init ?tid () =
      match tid with
      | None      -> pair ThreadID.null Nat.zero
      | Some tid  -> pair tid Nat.one

    let reify = Pair.reify ThreadID.reify Nat.reify

    let rec show n =
      let rec to_ground : Nat.logic -> int = function
      | Value (S n) -> 1 + (to_ground n)
      | Value (O)   -> 0
      | Var (i, _)  -> invalid_arg "Free Var"
      in
      try
        string_of_int @@ to_ground n
      with Invalid_argument _ ->
        match n with
        | Value (S n) ->
          Printf.sprintf "_.??{=/= 0}"
        | Var (i, []) ->
          Printf.sprintf "_.%d" i
        | Var (i, cs) ->
          let cs = String.concat "; " @@ List.map show cs in
          Printf.sprintf "_.%d{=/= %s}" i cs

    let pprint =
      let pp ff (tid, id) =
        Format.fprintf ff "{tid: %a; id: %s}" ThreadID.pprint tid (show id)
      in
      pprint_logic pp

    let nexto eid eid' tid =
      fresh (id id')
        (* TODO: check eid/tid is valid *)
        (eid  === pair tid id )
        (eid' === pair tid id')
        (id' === Nat.succ id)
  end

module Event =
  struct
    module T =
      struct
        @type ('eid, 'label) t =
          { eid   : 'eid
          ; label : 'label
          }
        with gmap

        let fmap f g x = GT.gmap(t) f g x
      end

    type tt = (EventID.tt, Label.tt) T.t
    type tl = inner MiniKanren.logic
      and inner = (EventID.tl, Label.tl) T.t
    type ti = (tt, tl) MiniKanren.injected

    include Fmap2(T)

    let event eid label = T.(inj @@ distrib @@ {eid; label})

    let reify = reify EventID.reify Lang.Label.reify

    let pprint =
      let pp ff { T.eid = eid; T.label = label } =
        Format.fprintf ff "@[<v>eid: %a; label: %a@]"
          EventID.pprint eid
          Lang.Label.pprint label
      in
      pprint_logic pp

  end

module EventSet =
  struct
    type tt = Event.tt List.ground
    type tl = inner MiniKanren.logic
      and inner = (Event.tl, tl) MiniKanren.Std.list
    type ti = (tt, tl) MiniKanren.injected

    type reified = (tt, tl) MiniKanren.reified

    let empty = nil

    let reify = List.reify Event.reify

    let pprint = pprint_llist Event.pprint

    let init mem =
      let helper (l, v) =
        let eid = EventID.init () in
        let label = Label.store !!MemOrder.SC (Loc.loc l) (Value.integer v) in
        Event.event eid label
      in
      List.list @@ List.map helper mem

    let mergeo = List.appendo
  end

module Ord =
  struct
    type tt = (EventID.tt * EventID.tt) List.ground
    type tl = inner MiniKanren.logic
      and inner = ((EventID.tl, EventID.tl) MiniKanren.Std.Pair.logic, tl) MiniKanren.Std.list
    type ti = (tt, tl) MiniKanren.injected

    let empty = nil

    let reify = List.reify (Pair.reify EventID.reify EventID.reify)

    let pprint =
      let helper ff (eid, eid') =
        Format.fprintf ff "@[(%a, %a)@]" EventID.pprint eid EventID.pprint eid'
      in
      pprint_llist (pprint_logic helper)

    let mergeo = List.appendo

    let trcl ord =
      let rec as_rel ord eid eid' =
        fresh (x ord')
          (ord === x % ord')
          (conde
            [ (x === pair eid eid')
            ; (as_rel ord' eid eid')
            ]
          )
      in
      let ordrelo = as_rel ord in
      let trcl_norec trcl_rec eid eid'' = conde
        [ (ordrelo eid eid'')
        ; fresh (eid')
            (ordrelo eid eid')
            (trcl_rec eid' eid'');
        ]
      in
      Tabling.(tabledrec two trcl_norec)

    let acyclico ord =
      let ord = trcl ord in
      let irreflex e =
        fresh (eid label)
          (e === Event.event eid label)
        ?~(ord eid eid)
      in
      Utils.list_all irreflex

    let rec totalo ord =
      let ord = trcl ord in
      let antisymm e e' =
        fresh (eid eid' label label')
          (e  === Event.event eid  label )
          (e' === Event.event eid' label')
          (conde
            [   (ord eid eid') &&& ?~(ord eid' eid)
            ; ?~(ord eid eid') &&&   (ord eid' eid)
            ]
          )
      in
      let rec helper es = conde
        [ (es === nil ())
        ; fresh (e es')
            (es === e % es')
            (Utils.list_all (antisymm e) es')
            (helper es')
        ]
      in
      helper
      (* fun es -> success *)

  end

module PO =
  struct
    include Ord

    let extendo po po' e =
      fresh (eid label x)
        (e === Event.event eid label)
        (po' === x % po)
        (conde
          [ (po === nil ()) &&& (x === pair (EventID.init ()) eid)
          ; fresh (eid' eid'' tl)
              (po === (pair eid'' eid') % tl)
              (x  === pair eid' eid)
          ]
        )

  end

module RF =
  struct
    include Ord

    let well_formedo es rf =
      let open Event in
      let rec helpero rs ws rf = conde [
        (rs === nil ()) &&& (rf === nil ());

        fresh (w r rs' rf' reid weid mo1 mo2 loc v)
          (r  === event reid @@ Label.load  mo1 loc v)
          (w  === event weid @@ Label.store mo2 loc v)
          (rs === r % rs')
          (rf === (pair weid reid) % rf')
          (List.membero ws w)
          (helpero rs' ws rf')
      ] in
      let loado e =
        fresh (eid mo loc v)
          (e === event eid (Label.load mo loc v))
      in
      let loado e b = conde [
        (b === Bool.truo ) &&&   (loado e);
        (b === Bool.falso) &&& ?~(loado e);
      ] in
      let storeo e =
        fresh (eid mo loc v)
          (e === event eid (Label.store mo loc v))
      in
      let storeo e b = conde [
        (b === Bool.truo ) &&&   (storeo e);
        (b === Bool.falso) &&& ?~(storeo e);
      ] in
      fresh (rs ws)
        (List.filtero loado  es rs)
        (List.filtero storeo es ws)
        (helpero rs ws rf)
  end

module ThreadPreExecution =
  struct
    module T =
      struct
        @type ('eid, 'es, 'po) t =
          { eid : 'eid
          ; es  : 'es
          ; po  : 'po
          }
        with gmap

        let fmap f g h x = GT.gmap(t) f g h x
      end

    type tt = (EventID.tt, EventSet.tt, PO.tt) T.t
    type tl = inner MiniKanren.logic
      and inner = (EventID.tl, EventSet.tl, PO.tl) T.t
    type ti = (tt, tl) MiniKanren.injected

    include Fmap3(T)

    let prexec eid es po = T.(inj @@ distrib @@ {eid; es; po})

    let init ?tid () =
      let eid = EventID.init ?tid () in
      let es  = EventSet.empty () in
      let po  = PO.empty () in
      prexec eid es po

    let reify = reify EventID.reify EventSet.reify PO.reify

    let pprint =
      let pp ff = let open T in fun {es; po} ->
        Format.fprintf ff "@[<v>Events: %a@;po: %a@;@]"
          EventSet.pprint es
          Ord.pprint po
      in
      pprint_logic pp

    let eventso t es =
      fresh (eid po)
        (t === prexec eid es po)

    let program_ordero t po =
      fresh (eid es)
        (t === prexec eid es po)

    let stepo tid label t t' = conde
      [ (label === Label.empty ()) &&& (t === t')

      ; fresh (eid eid' es es' po po' e)
          (t   === prexec eid  es  po )
          (t'  === prexec eid' es' po')
          (e   === Event.event eid label)
          (es' === e % es)
          (label =/= Label.empty ())
        ?~(fresh (mo loc) (label === Label.error (Error.datarace mo loc)))
          (EventID.nexto eid eid' tid)
          (PO.extendo po po' e)
      ]

  end

module PreExecution =
  struct
    module TLS = Lang.ThreadLocalStorage(ThreadPreExecution)

    type tt = EventSet.tt * TLS.tt
    type tl = inner MiniKanren.logic
      and inner = EventSet.tl * TLS.tl
    type ti = (tt, tl) MiniKanren.injected

    let reify = Pair.reify EventSet.reify TLS.reify

    let pprint =
      let pp ff (_, thrds) = TLS.pprint ff thrds in
      pprint_logic pp

    let init ~thrdn mem =
      let ies = EventSet.init mem in
      let prexecs =
        TLS.initi thrdn (fun tid -> ThreadPreExecution.init ~tid ())
      in
      pair ies prexecs

    let alloc ~thrdn locs =
      init ~thrdn @@ List.map (fun l -> (l, 0)) locs

    let checko t lvs = success

    let stepo tid label t t' =
      fresh (ies pes pes' prexec prexec')
        (t  === pair ies pes )
        (t' === pair ies pes')
        (TLS.geto pes      tid prexec )
        (TLS.seto pes pes' tid prexec')
        (ThreadPreExecution.stepo tid label prexec prexec')

    let parallelo pe1 pe2 pe' =
      fresh (eid1 eid2 es1 es2 es' po1 po2 po')
        (pe1 === ThreadPreExecution.prexec eid1 es1 po1)
        (pe2 === ThreadPreExecution.prexec eid2 es2 po2)
        (pe' === ThreadPreExecution.prexec (EventID.init ()) es' po')
        (EventSet.mergeo es1 es2 es')
        (PO.mergeo po1 po2 po')

    let execo t es po rf =
      fresh (ies pes pe es')
        (t === pair ies pes)
        (* merge [es] and [po] of parallel threads *)
        (TLS.foldo parallelo pes (ThreadPreExecution.init ()) pe)
        (* extract [es] and [po] *)
        (ThreadPreExecution.eventso pe es')
        (ThreadPreExecution.program_ordero pe po)
        (* add initialize events to the set of all events *)
        (EventSet.mergeo ies es' es)
        (* build [rf] *)
        (RF.well_formedo es rf)

  end

module SequentialConsistent =
  struct
    let consistento t =
      fresh (es po rf sc)
        (PreExecution.execo t es po rf)
        (* build and check [sc] *)
        (Ord.mergeo po rf sc)
        (Ord.totalo sc es)
  end
