open MiniKanren
open Memory
open Lang
open Utils

module Error =
  struct
    type tt =
      | DataRace
      | AssertionFailed

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    let reify = MiniKanren.reify

    let to_string = function
      | DataRace        -> "datarace"
      | AssertionFailed -> "assert failed"

    let show = GT.show(logic) (to_string)
  end

module type Memory =
  sig
    include Utils.Logic

    val init : mem:(string * int) list -> ti

    val shapeo : ti -> Lang.Loc.ti list -> MiniKanren.goal

    val checko : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val stepo : Lang.ThreadID.ti -> Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end

module State (M : Memory) :
  sig
    include Utils.Logic

    val mem   : M.ti -> ti
    val error : Error.ti -> M.ti -> ti

    val stepo : Lang.ThreadID.ti -> Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end =
  struct
    module T = struct
      @type ('m, 'err) t =
        | Mem   of 'm
        | Error of 'err * 'm
      with gmap

      let fmap fa fb x = GT.gmap(t) fa fb x
    end

    type tt = (M.tt, Error.tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (M.tl, Error.tl) T.t

    type ti = (tt, tl) Semantics.Term.ti

    module F = Fmap2(T)

    let mem m     = inj @@ F.distrib @@ T.Mem m
    let error e m = inj @@ F.distrib @@ T.Error (e, m)

    let rec reify h = F.reify M.reify Error.reify h

    let pprint =
      let pp ff = T.(function
        | Mem m ->
          Format.fprintf ff "%a" M.pprint m
        | Error (e, m) ->
          Format.fprintf ff "@[<v>Error:@;<1 2>%s@;Memory:@;<1 2>%a@;@]" (Error.show e) M.pprint m
      )
      in
      pprint_logic pp

    let stepo tid label t t' =
      fresh (m m')
        (t === mem m)
        (conde [
          (t' === error !!Error.AssertionFailed m) &&& (label === Label.assert_fail ());

          fresh (mo loc)
            (t' === error !!Error.DataRace m')
            (label === Label.datarace mo loc)
            (M.stepo tid label m m');

          (t' === mem m') &&& ?~(
            fresh (mo loc)
              (label === Label.datarace mo loc)
          ) &&& (M.stepo tid label m m')
        ])
  end

module type T =
  sig
    module Memory : Memory

    module State : module type of State(Memory)

    module Node : module type of Semantics.MakeConfig(Lang.ThreadSubSys)(State)

    val intrpo : (Lang.ThreadSubSys.tt, State.tt, State.tt, Lang.ThreadSubSys.tl, State.tl, State.tl) Semantics.interpreter
  end

module Make (M : Memory) =
  struct
    module Memory = M

    module State = State(Memory)

    module Node = Semantics.MakeConfig(Lang.ThreadSubSys)(State)

    open Lang

    let stepo t t' =
      fresh (tsys tsys' s s' tid label)
        (t  === Node.cfg tsys  s )
        (t' === Node.cfg tsys' s')
        (ThreadSubSys.stepo tid label tsys tsys')
        (State.stepo tid label s s')

    let evalo =
      let irreducibleo t =
        fresh (tsys s)
          (t === Node.cfg tsys s)
          (ThreadSubSys.terminatedo tsys)
      in
      Semantics.Reduction.make_eval ~irreducibleo stepo

    let intrpo p i o =
      fresh (s s' p')
        (s  === Node.cfg p  i)
        (s' === Node.cfg p' o)
        (evalo s s')

  end

module MemorySC =
  struct
    type tt = ValueStorage.tt

    type tl = ValueStorage.tl
      and inner = ValueStorage.inner

    type ti = ValueStorage.ti

    let reify = ValueStorage.reify

    let init ~mem =
      let mem = List.map (fun (l, v) -> (Loc.loc l, Value.integer v)) mem in
      ValueStorage.from_assoc mem

    let pprint = ValueStorage.pprint

    let spawno t t' tid1 tid2 = success

    let joino t t' tid1 tid2 = success

    let load_sco t t' tid loc value =
      (t === t') &&&
      (ValueStorage.reado t loc value)

    let store_sco t t' tid loc value =
      (ValueStorage.writeo t t' loc value)

    let cas_sco t t' tid loc expected desired value =
      (ValueStorage.reado t loc value) &&&
      (conde [
        (Lang.Value.eqo value expected) &&& (ValueStorage.writeo t t' loc desired);
        (Lang.Value.nqo value expected) &&& (t === t');
      ])

    let shapeo t locs =
      (ValueStorage.shapeo t locs)

    let checko t loc v =
      (ValueStorage.reado t loc v)

    let stepo tid label t t' = conde [
      (label === Label.empty ()) &&& (t === t');

      fresh (tid1 tid2)
        (label === Label.spawn tid1 tid2)
        (spawno t t' tid1 tid2);

      fresh (tid1 tid2)
        (label === Label.join tid1 tid2)
        (joino t t' tid1 tid2);

      fresh (mo loc v)
        (label === Label.load !!MemOrder.SC loc v)
        (load_sco t t' tid loc v);

      fresh (mo loc v)
        (label === Label.store !!MemOrder.SC loc v)
        (store_sco t t' tid loc v);

      fresh (loc e d v)
        (label === Label.cas !!MemOrder.SC !!MemOrder.SC loc e d v)
        (cas_sco t t' tid loc e d v);
    ]
  end

module SequentialConsistent = Make(MemorySC)

module MemoryRA : Memory =
  struct
    module T = struct
      type ('a, 'b, 'c, 'd) t = {
        thrds : 'a;
        story : 'b;
        na    : 'c;
        sc    : 'd;
      }

      let fmap fa fb fc fd {thrds; story; na; sc} = {
        thrds = fa thrds;
        story = fb story;
        na = fc na;
        sc = fd sc;
      }
    end

    type tt = (
      (ThreadID.tt, ThreadFront.tt) Storage.tt,
      MemStory.tt,
      ViewFront.tt,
      ViewFront.tt
    ) T.t

    type tl = inner MiniKanren.logic
      and inner = (
        (ThreadID.tl, ThreadFront.tl) Storage.tl,
        MemStory.tl,
        ViewFront.tl,
        ViewFront.tl
      ) T.t

    type ti = (tt, tl) MiniKanren.injected

    include Fmap4(T)

    let state thrds story na sc = inj @@ distrib @@ T.({thrds; story; na; sc;})

    let reify = reify (Storage.reify ThreadID.reify ThreadFront.reify) (MemStory.reify) (ViewFront.reify) (ViewFront.reify)

    let init ~mem =
      let mem   = List.map (fun (l, v) -> (Loc.loc l, Value.integer v)) mem in
      let locs  = List.map fst mem in
      let thrd  = ThreadFront.allocate locs in
      let thrds = Storage.from_assoc [(ThreadID.fst, thrd)] in
      let story = MemStory.init mem in
      let na    = ViewFront.allocate locs in
      let sc    = ViewFront.allocate locs in
      state thrds story na sc

    let pprint =
      let pp ff = let open T in fun {thrds; story; na; sc} ->
        Format.fprintf ff "@[<v>%a@;%a@;@[<v>NA-front :@;<1 4>%a@;@]@;@[<v>SC-front :@;<1 4>%a@;@]@]"
          (Storage.pprint (fun ff (tid, thrd) ->
            Format.fprintf ff "@[<v>Thread #%a:@;%a@]" ThreadID.pprint tid ThreadFront.pprint thrd
          )) thrds
          MemStory.pprint story
          ViewFront.pprint na
          ViewFront.pprint sc
      in
      pprint_logic pp

    let spawno t t' pid tid1 tid2 =
      fresh (thrds thrds' thrds'' pfront tfront1 tfront2 story na sc)
        (t  === state thrds   story na sc)
        (t' === state thrds'' story na sc)
        (Storage.geto thrds pid pfront)
        (ThreadFront.spawno pfront tfront1 tfront2)
        (Storage.extendo thrds  thrds'  tid1 tfront1)
        (Storage.extendo thrds' thrds'' tid2 tfront2)

    let joino t t' pid tid1 tid2 =
      fresh (thrds thrds' story na sc pfront pfront' tfront1 tfront2)
        (t  === state thrds  story na sc)
        (t' === state thrds' story na sc)
        (Storage.geto thrds pid  pfront )
        (Storage.geto thrds tid1 tfront1)
        (Storage.geto thrds tid2 tfront2)
        (Storage.seto thrds thrds' pid pfront')
        (ThreadFront.joino pfront pfront' tfront1 tfront2)

    let na_awareo na loc last_ts = Timestamp.(
      fresh (na_ts)
        (ViewFront.tso na loc na_ts)
        (na_ts <= last_ts)
    )

    let na_stucko na loc last_ts = Timestamp.(
      fresh (na_ts)
        (ViewFront.tso na loc na_ts)
        (na_ts > last_ts)
    )

    let load_nao t t' tid loc value =
      fresh (thrds story na sc thrd ts vf)
        (t === t')
        (t === state thrds story na sc)
        (Storage.geto thrds tid thrd)
        (ThreadFront.tso thrd loc ts)
        (MemStory.last_tso story loc ts)
        (na_awareo na loc ts)
        (MemStory.loado story loc ts ts value vf)

    let store_nao t t' tid loc value =
      fresh (thrds thrds' story story' na na' sc thrd thrd' ts ts' rel vf)
        (t  === state thrds  story  na  sc)
        (t' === state thrds' story' na' sc)
        (Storage.geto thrds        tid thrd)
        (Storage.seto thrds thrds' tid thrd')
        (ThreadFront.tso thrd loc ts)
        (MemStory.last_tso story loc ts)
        (na_awareo na loc ts)

        (MemStory.next_tso story loc ts')
        (ThreadFront.updateo thrd thrd' loc ts')
        (ViewFront.updateo na na' loc ts')
        (MemStory.storeo story story' loc value (ViewFront.bottom ()))

    let not_last_tso story loc ts =
      fresh (last_ts)
        (ts =/= last_ts)
        (MemStory.last_tso story loc last_ts)

    let na_dataraceo t t' tid loc =
      fresh (thrds story na sc thrd ts)
        (t === t')
        (t === state thrds story na sc)
        (Storage.geto thrds tid thrd)
        (ThreadFront.tso thrd loc ts)
        (conde [
          (na_stucko na loc ts);
          (not_last_tso story loc ts);
        ])

    let dataraceo t t' tid loc =
      fresh (thrds story na sc thrd ts)
        (t === t')
        (t === state thrds story na sc)
        (Storage.geto thrds tid thrd)
        (ThreadFront.tso thrd loc ts)
        (na_stucko na loc ts)

    let load_rlxo t t' tid loc value ts =
      fresh (thrds thrds' story story' na sc thrd thrd' thrd'' last_ts vf)
        (t  === state thrds  story na sc)
        (t' === state thrds' story na sc)
        (Storage.geto thrds        tid thrd  )
        (Storage.seto thrds thrds' tid thrd'')
        (ThreadFront.tso thrd loc last_ts)
        (na_awareo na loc last_ts)
        (MemStory.loado story loc last_ts ts value vf)
        (ThreadFront.update_acqo thrd thrd' vf)
        (ThreadFront.updateo thrd' thrd'' loc ts)

    let store_rlxo t t' tid loc value ts =
      fresh (thrds thrds' story story' na sc thrd thrd' last_ts rel)
        (t  === state thrds  story  na sc)
        (t' === state thrds' story' na sc)
        (Storage.geto thrds        tid thrd)
        (Storage.seto thrds thrds' tid thrd')
        (ThreadFront.tso thrd loc last_ts)
        (na_awareo na loc last_ts)
        (MemStory.next_tso story loc ts)
        (ThreadFront.front_relo thrd loc rel)
        (ThreadFront.updateo thrd thrd' loc ts)
        (MemStory.storeo story story' loc value rel)

    let fence_acqo t t' tid =
      fresh (thrds thrds' story thrd thrd' na sc)
        (t  === state  thrds  story na sc)
        (t' === state  thrds' story na sc)
        (Storage.geto thrds        tid thrd )
        (Storage.seto thrds thrds' tid thrd')
        (ThreadFront.fence_acqo  thrd thrd')

    let fence_relo ?loc t t' tid =
      fresh (thrds thrds' thrd thrd' story na sc)
        (t  === state  thrds  story na sc)
        (t' === state  thrds' story na sc)
        (Storage.geto thrds       tid thrd )
        (Storage.seto thrds thrds' tid thrd')
        (ThreadFront.fence_relo  thrd thrd' ?loc)

    let load_acqo t t'' tid loc value ts =
      fresh (t')
        (load_rlxo t t' tid loc value ts)
        (fence_acqo t' t'' tid)

    let store_relo t t'' tid loc value ts =
      fresh (t')
        (fence_relo t t' tid ~loc)
        (store_rlxo t' t'' tid loc value ts)

    let load_sco t t' tid loc value ts = Timestamp.(
      fresh (thrds story na sc sc_ts ts)
        (t === state thrds story na sc)
        (load_acqo t t' tid loc value ts)
        (ViewFront.tso sc loc sc_ts)
        (sc_ts <= ts)
    )

    let store_sco t t'' tid loc value ts =
      fresh (t' thrds story na sc sc' ts)
        (t'   === state thrds story na sc )
        (t''  === state thrds story na sc')
        (store_relo t t' tid loc value ts)
        (ViewFront.updateo sc sc' loc ts)

    let load_rlxo t t' tid loc value =
      fresh (ts)
        (load_rlxo t t' tid loc value ts)

    let store_rlxo t t' tid loc value =
      fresh (ts)
        (store_rlxo t t' tid loc value ts)

    let load_acqo t t' tid loc value =
      fresh (ts)
        (load_acqo t t' tid loc value ts)

    let store_relo t t' tid loc value =
      fresh (ts)
        (store_relo t t' tid loc value ts)

    let load_sco t t' tid loc value =
      fresh (ts)
        (load_sco t t' tid loc value ts)

    let store_sco t t' tid loc value =
      fresh (ts)
        (store_sco t t' tid loc value ts)

    let cas_sco t t' tid loc expected desired value = success

    let shapeo t locs =
      fresh (tree story na sc)
        (t === state tree story na sc)
        (MemStory.shapeo story locs)
        (ViewFront.shapeo na locs)
        (ViewFront.shapeo sc locs)

    let checko t loc v =
      fresh (tree story na sc)
        (t === state tree story na sc)
        (MemStory.last_valueo story loc v)

    let stepo tid label t t' = conde [
      (label === Label.empty ()) &&& (t === t');

      fresh (tid1 tid2)
        (label === Label.spawn tid1 tid2)
        (spawno t t' tid tid1 tid2);

      fresh (tid1 tid2)
        (label === Label.join tid1 tid2)
        (joino t t' tid tid1 tid2);

      fresh (mo loc v)
        (label === Label.load mo loc v)
        (conde [
          (* (mo === !!MemOrder.SC ) &&& (load_sco  t'' t' tid loc v); *)
          (mo === !!MemOrder.ACQ) &&& (load_acqo t t' tid loc v);
          (mo === !!MemOrder.RLX) &&& (load_rlxo t t' tid loc v);
          (mo === !!MemOrder.NA ) &&& (load_nao  t t' tid loc v);
        ]);

      fresh (mo loc v)
        (label === Label.store mo loc v)
        (conde [
          (* (mo === !!MemOrder.SC ) &&& (store_sco  t t' tid loc v); *)
          (mo === !!MemOrder.REL) &&& (store_relo t t' tid loc v);
          (mo === !!MemOrder.RLX) &&& (store_rlxo t t' tid loc v);
          (mo === !!MemOrder.NA ) &&& (store_nao  t t' tid loc v);
        ]);

      fresh (loc mo v)
        (label === Label.datarace mo loc)
        (conde [
          (conde [
            (mo === !!MemOrder.SC);
            (mo === !!MemOrder.ACQ);
            (mo === !!MemOrder.REL);
            (mo === !!MemOrder.RLX);
          ]) &&&
          (dataraceo t t' tid loc);

          (mo === !!MemOrder.NA) &&&
          (na_dataraceo t t' tid loc);
        ]);
    ]
  end

module ReleaseAcquire = Make(MemoryRA)
