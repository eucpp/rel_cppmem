open MiniKanren
open MiniKanren.Std

open Lang
open Utils

module EventID =
  struct
    type tt = ThreadID.ti * Std.Nat.ground

    type tl = inner MiniKanren.logic
      and inner = ThreadID.tl * Std.Nat.logic

    type ti = (tt, tl) MiniKanren.injected

    let init tid = pair tid Nat.one

    let reify = Pair.reify ThreadID.reify Nat.reify

    let rec show n =
      let rec to_ground : tl -> int = function
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
        Format.fprintf ff "{tid: %a; id: %s}" (ThreadID.pprint tid) (show id)
      in
      pprint_logic pp

    let nexto eid eid' tid =
      fresh (id id')
        (eid === pair tid id )
        (eid === pair tid id')
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

module Order =
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
          (e  === Event.event eid  label)
          (e' === Event.event eid' label)
          (conde
            [   (ord eid eid') &&& ?~(ord eid' eid)
            ; ?~(ord eid eid') &&&   (ord eid' eid)
            ]
          )
      in
      let rec helper es = conde
        [ (es === nil ())
        ; fresh ()
            (es === e % es')
            (Utils.list_all (antisymm e) es')
            (helper es')
        ]
      in
      helper

  end

module PO = Order

module RF =
  struct
    include Order

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
        fresh (eid tid mo loc v)
          (e === event eid (Label.load tid mo loc v))
      in
      let loado e b = conde [
        (b === Bool.truo ) &&&   (loado e);
        (b === Bool.falso) &&& ?~(loado e);
      ] in
      let storeo e =
        fresh (eid tid mo loc v)
          (e === event eid (Label.store tid mo loc v))
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

    let mergeo = List.appendo
  end

module ThreadPreExecution =
  struct
    module T =
      struct
        @type ('eid, 'es, 'ord) t =
          { eid : 'eid
          ; es  : 'es
          ; po  : 'ord
          }
        with gmap

        let fmap f g h x = GT.gmap(t) f g h x
      end

    type tt = (Event.tt, EventSet.tt, Order.tt) T.t
    type tl = inner MiniKanren.logic
      and inner = (Event.tl, EventSet.tl, Order.tl) T.t
    type ti = (tt, tl) MiniKanren.injected

    include Fmap3(T)

    let prexec eid es po = T.(inj @@ distrib @@ {eid; es; po})

    let init tid =
      let eid = EvenID.init tid in
      let es  = EventSet.empty () in
      let po  = Order.empty () in
      prexec eid es po

    let reify h = reify Event.reify EventSet.reify Order.reify h

    let pprint =
      let pp ff = let open T in {es; po} =
        Format.fprintf ff "@[<v>Events: %a@;@]@[<v>po: %a @;@]@."
          EventSet.pprint es
          Order.pprint po
      in
      pprint_logic pp

    let eventso t es =
      fresh (eid po)
        (t === prexec eid es po)

    let program_ordero t po =
      fresh (eid es)
        (t === prexec eid es po)

    let stepo tid label t t' = conde
      [ (label === Label.empty ()) &&& (t === t');

      ; fresh (eid eid' es es' po po' e)
          (t   === prexec eid  es  po )
          (t'  === prexec eid' es' po')
          (e   === Event.event eid label)
          (es' === e % es)
          (EventID.nexto eid eid' tid)
          (PO.extendo po po' e)
      ]

    let parallelo t1 t2 t' =
      fresh ()
      ()
  end

module PreExecution =
  struct
    module TLS = Lang.ThreadLocalStorage(ThreadPreExecution)

    type tt = EventSet.tt * TLS.tt
    type tl = inner MiniKanren.logic
      and inner = EventSet.tl * TLS.tl
    type ti = (tt, tl) MiniKanren.injected

    let prexec eid es po = T.(inj @@ distrib @@ {eid; es; po})

    let init tid =
      let eid = EvenID.init tid in
      let es  = EventSet.empty () in
      let po  = Order.empty () in
      prexec eid es po

    let reify = Pair.reify EventSet.reify TLS.reify

    let pprint =
      let pp ff (_, thrds) = TLS.pprint thrds in
      pprint_logic pp

    let init ~thrdn ~mem =
      let ies = EventSet.init mem in
      let prexecs =
        TLS.initi thrdn (fun i -> ThreadPreExecution.init @@ ThreadID.tid i)
      in
      pair ies prexecs

    let stepo tid label t t' =
      fresh (ies pes pes' prexec prexec')
        (t  === pair ies pes )
        (t' === pair ies pes')
        (TLS.geto pes      tid prexec )
        (TLS.seto pes pes' tid prexec')
        (ThreadPreExecution.stepo tid label prexec prexec')

    let paralello pe1 pe2 pe' =
      fresh (tid1 tid2 es1 es2 es' po1 po2 po')
        (pe1 === ThreadPreExecution.prexec tid1 es1 po1)
        (pe2 === ThreadPreExecution.prexec tid2 es2 po2)
        (pe' === ThreadPreExecution.prexec ThreadID.init es' po')
        (EventSet.mergeo es1 es2 es')
        (PO.mergeo po1 po2 po')

    let initilizeo ies pe pe' =
      fresh (tid es po po')
        (pe  === ThreadPreExecution.prexec tid es po )
        (pe' === ThreadPreExecution.prexec tid es po')
        (PO.initilizeo po po' ies)

    let execo t es po rf =
      fresh (ies pes pes' pe es es' po rf)
        (t === pair ies pes)
        (* add initialize events to each thread [po] *)
        (mapo (initilizeo ies) pes pes')
        (* merge [es] and [po] of parallel threads *)
        (foldo parallelo pes' (empty ()) pe)
        (* extract [es] and [po] *)
        (PreExecution.eventso pe es)
        (PreExecution.program_ordero pe po)
        (* add initialize events to the set of all events *)
        (EventSet.mergeo ies es es')
        (* build [rf] *)
        (RF.well_formedo es' rf)

    let consistento t = success
        (* build and check [sc] *)
        (* (Ord.mergeo po rf sc)
        (Ord.totalo sc es') *)

  end
