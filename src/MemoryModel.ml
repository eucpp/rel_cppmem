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

    val init : regs:string list -> mem:(string * int) list -> ti

    val get_regso : ti ->       Lang.ThreadID.ti -> Memory.RegisterStorage.ti -> MiniKanren.goal
    val set_regso : ti -> ti -> Lang.ThreadID.ti -> Memory.RegisterStorage.ti -> MiniKanren.goal

    val shapeo : ti -> Lang.Loc.ti list -> MiniKanren.goal

    val checko : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val transitiono : Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end

module State (M : Memory) :
  sig
    include Utils.Logic

    val mem   : M.ti -> ti
    val error : Error.ti -> M.ti -> ti

    val get_regso : ti ->       Lang.ThreadID.ti -> Memory.RegisterStorage.ti -> MiniKanren.goal
    val set_regso : ti -> ti -> Lang.ThreadID.ti -> Memory.RegisterStorage.ti -> MiniKanren.goal

    val transitiono : Lang.Label.ti -> ti -> ti -> MiniKanren.goal
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

    let get_regso t thrdId rs =
      fresh (m)
        (t === mem m)
        (M.get_regso m thrdId rs)

    let set_regso t t' thrdId rs = conde [
      fresh (m m')
        (t  === mem m )
        (t' === mem m')
        (M.set_regso m m' thrdId rs);

      fresh (err m m')
        (t  === error err m )
        (t' === error err m')
        (M.set_regso m m' thrdId rs);
    ]

    let transitiono label t t' =
      fresh (m m')
        (t === mem m)
        (conde [
          (t' === error !!Error.AssertionFailed m) &&& (label === Label.assert_fail ());

          fresh (thrdId mo loc)
            (t' === error !!Error.DataRace m')
            (label === Label.datarace thrdId mo loc)
            (M.transitiono label m m');

          (t' === mem m') &&& ?~(
            fresh (thrdId mo loc)
              (label === Label.datarace thrdId mo loc)
          ) &&& (M.transitiono label m m')
        ])
  end

module type T =
  sig
    module Memory : Memory

    module State : module type of State(Memory)

    module Node : module type of Semantics.MakeConfig(Lang.Term)(State)

    val intrpo : (Lang.Term.tt, State.tt, State.tt, Lang.Term.tl, State.tl, State.tl) Semantics.interpreter
  end

module Make (M : Memory) =
  struct
    module Memory = M

    module State = State(Memory)

    module Node = Semantics.MakeConfig(Lang.Term)(State)

    let lift_split splito term ctx rdx =
      fresh (term' ctx' rdx' state rs thrdId)
        (term === Node.cfg term' state)
        (rdx  === Node.cfg rdx'  state)
        (ctx  === Rules.Context.context ctx' rs)
        (splito term' ctx' rdx')
        (Lang.Context.thrdIdo ctx' thrdId)
        (State.get_regso state thrdId rs)

    let lift_plug plugo ctx rdx term =
      fresh (term' ctx' rdx' state state' thrdId rs)
        (term === Node.cfg term' state')
        (rdx  === Node.cfg rdx'  state )
        (ctx  === Rules.Context.context ctx' rs)
        (plugo ctx' rdx' term')
        (Lang.Context.thrdIdo ctx' thrdId)
        (State.set_regso state state' thrdId rs)

    let lift_rule rule ctx ctx' t t' =
      fresh (label prog prog' state state')
        (t  === Node.cfg prog  state )
        (t' === Node.cfg prog' state')
        (rule label ctx ctx' prog prog')
        (State.transitiono label state state')

    (* let thrd_local_rules = List.map lift_rule (Rules.Basic.all)
    let thrd_inter_rules = List.map lift_rule (Rules.ThreadSpawning.all @ Rules.Atomic.all)

    let thrd_local_evalo =
      let evalo_norec evalo thrdId t t'' =
        let splito           = lift_split @@ Lang.thrd_splito thrdId in
        let plugo            = lift_plug Lang.plugo in
        let irreducibleo     = Node.lift_tpred Lang.Term.irreducibleo in
        let rdx_irreducibleo = Node.lift_tpred (fun t -> conde [
          (Lang.Term.irreducibleo t);
          (Lang.Term.thrd_inter_termo t);
        ]) in
        conde [
          ((irreducibleo t) &&& (t === t''));

          fresh (ctx rdx)
            (splito t ctx rdx)
            (conde [
              (rdx_irreducibleo rdx) &&& (t === t'');

              fresh (ctx' rdx' t')
                (plugo ctx' rdx' t')
                (conde @@ List.map (fun rule -> rule ctx ctx' rdx rdx') thrd_local_rules)
                (evalo thrdId t' t'');
            ]);
        ]
      in
      Tabling.(tabledrec three) evalo_norec

  let thrd_inter_stepo thrdId = Semantics.Reduction.make_step
    (lift_split @@ thrd_splito thrdId)
    (lift_plug Lang.plugo)
    (List.map lift_rule (Rules.ThreadSpawning.all @ Rules.Atomic.all))

  let stepo t t'' =
    let splito = lift_split @@ Lang.splito in
    let plugo  = lift_plug Lang.plugo in
    fresh (ctx ctx' rdx rdx' thrdId t')
      (splito t ctx rdx)
      (plugo ctx' rdx' t')
      (conde [
          (conde @@ List.map (fun rule -> rule ctx ctx' rdx rdx') thrd_local_rules) &&&
          (Rules.Context.thrdIdo ctx thrdId) &&&
          (thrd_local_evalo thrdId t' t'');

        (conde @@ List.map (fun rule -> rule ctx ctx' rdx rdx') thrd_inter_rules) &&&
        (t' === t'');
      ]) *)

  (* let stepo t t' =
    conde [
    fresh (thrdId)
      (thrd_inter_stepo thrdId t t');
    fresh (thrdId)
      (thrd_local_evalo thrdId t t');
  ] *)

  let stepo = Semantics.Reduction.make_step
    (lift_split Lang.splito)
    (lift_plug Lang.plugo)
    (List.map lift_rule (Rules.Basic.all @ Rules.Atomic.all @ Rules.ThreadSpawning.all))

  let evalo =
    let irreducibleo t =
      fresh (prog state)
        (t === Node.cfg prog state)
        (Lang.Term.irreducibleo prog)
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
    module T = struct
      type ('a, 'b) t = {
        thrds : 'a;
        sc    : 'b;
      }

      let fmap fa fb {thrds; sc} = {
        thrds = fa thrds;
        sc = fb sc;
      }
    end

    module TLS = ThreadLocalStorage(RegisterStorage)

    type tt = (
      TLS.tt,
      ValueStorage.tt
    ) T.t

    type tl = inner MiniKanren.logic
      and inner = (
        TLS.tl,
        ValueStorage.tl
      ) T.t

    type ti = (tt, tl) MiniKanren.injected

    type lt = Lang.Label.tt
    type ll = Lang.Label.tl
      and linner = Lang.Label.inner
    type li = Lang.Label.ti

    include Fmap2(T)

    let state thrds sc = inj @@ distrib @@ T.({thrds; sc;})

    let reify = reify (TLS.reify) (ValueStorage.reify)

    let init ~regs ~mem =
      let mem   = List.map (fun (l, v) -> (Loc.loc l, Value.integer v)) mem in
      let regs  = List.map Register.reg regs in
      let thrd  = RegisterStorage.allocate regs in
      let thrds = TLS.leaf thrd in
      let sc    = ValueStorage.from_assoc mem in
      state thrds sc

    let pprint =
      let pp ff {T.thrds = thrds; T.sc = sc;} =
        Format.fprintf ff "@[<v>%a@;@[<v>Memory :@;<1 4>%a@;@]@]"
          TLS.pprint thrds
          ValueStorage.pprint sc
      in
      pprint_logic pp

    let get_thrdo t thrdId thrd =
      fresh (tree sc)
        (t === state tree sc)
        (TLS.geto tree thrdId thrd)

    let set_thrdo t t' thrdId thrd =
      fresh (tree tree' sc)
        (t  === state tree  sc)
        (t' === state tree' sc)
        (TLS.seto tree tree' thrdId thrd)

    let regwriteo t t' thrdId var value =
      fresh (thrd thrd')
        (get_thrdo t    thrdId thrd )
        (set_thrdo t t' thrdId thrd')
        (RegisterStorage.writeo thrd thrd' var value)

    let spawno t t' thrdId =
      fresh (tree tree' sc)
        (t  === state tree  sc)
        (t' === state tree' sc)
        (TLS.spawno tree tree' thrdId)

    let joino t t' thrdId =
      fresh (tree tree' sc)
        (t  === state tree  sc)
        (t' === state tree' sc)
        (TLS.joino tree tree' thrdId)

    let returno t t' thrdId rs =
      let rec helper thrd pthrd pthrd'' regs = conde [
        (regs === Std.nil ()) &&& (pthrd === pthrd'');

        fresh (r rs pthrd' v)
          (regs === Std.List.conso r rs)
          (RegisterStorage.reado  thrd r v)
          (RegisterStorage.writeo pthrd pthrd' r v)
          (helper thrd pthrd' pthrd'' rs);
      ] in
      fresh (tree sc parentThrdId thrd pthrd pthrd')
        (t  === state tree sc)
        (ThreadID.parento parentThrdId thrdId)
        (get_thrdo t    thrdId       thrd  )
        (get_thrdo t    parentThrdId pthrd )
        (set_thrdo t t' parentThrdId pthrd')
        (helper thrd pthrd pthrd' rs)

    let load_sco t t' thrdId loc value =
      fresh (tree sc)
        (t === t')
        (t === state tree sc)
        (ValueStorage.reado sc loc value)

    let store_sco t t' thrdId loc value =
      fresh (tree sc sc')
        (t  === state tree sc )
        (t' === state tree sc')
        (ValueStorage.writeo sc sc' loc value)

    let cas_sco t t' thrdId loc expected desired value =
    fresh (tree sc sc' b)
      (t  === state tree sc )
      (t' === state tree sc')
      (ValueStorage.reado sc loc value)
      (conde [
        (Lang.Value.eqo value expected) &&& (ValueStorage.writeo sc sc' loc desired);
        (Lang.Value.nqo value expected) &&& (sc === sc');
      ])

    let get_regso = get_thrdo

    let set_regso = set_thrdo

    let shapeo t locs =
      fresh (tree story na sc)
        (t === state tree sc)
        (ValueStorage.shapeo sc locs)

    let checko t loc v =
      fresh (tree sc)
        (t === state tree sc)
        (ValueStorage.reado sc loc v)

    let transitiono label t t' = conde [
      (label === Label.empty ()) &&& (t === t');

      fresh (thrdId)
        (label === Label.spawn thrdId)
        (spawno t t' thrdId);

      fresh (thrdId)
        (label === Label.join thrdId)
        (joino t t' thrdId);

      fresh (thrdId rs)
        (label === Label.return thrdId rs)
        (returno t t' thrdId rs);

      fresh (thrdId reg v)
        (label === Label.regwrite thrdId reg v)
        (regwriteo t t' thrdId reg v);

      fresh (thrdId mo loc v)
        (label === Label.load thrdId !!MemOrder.SC loc v)
        (* (mo =/= !!MemOrder.NA) *)
        (load_sco t t' thrdId loc v);

      fresh (thrdId mo loc v)
        (label === Label.store thrdId !!MemOrder.SC loc v)
        (* (mo =/= !!MemOrder.NA) *)
        (store_sco t t' thrdId loc v);

      fresh (thrdId loc e d v)
        (label === Label.cas thrdId !!MemOrder.SC !!MemOrder.SC loc e d v)
        (cas_sco t t' thrdId loc e d v);

      (* fresh (thrdId loc)
        (t === t')
        (label === Label.datarace thrdId !!MemOrder.NA loc); *)
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

    module TLS = ThreadLocalStorage(ThreadFront)

    type tt = (
      TLS.tt,
      MemStory.tt,
      ViewFront.tt,
      ViewFront.tt
    ) T.t

    type tl = inner MiniKanren.logic
      and inner = (
        TLS.tl,
        MemStory.tl,
        ViewFront.tl,
        ViewFront.tl
      ) T.t

    type ti = (tt, tl) MiniKanren.injected

    type lt = Lang.Label.tt
    type ll = Lang.Label.tl
      and linner = Lang.Label.inner
    type li = Lang.Label.ti

    include Fmap4(T)

    let state thrds story na sc = inj @@ distrib @@ T.({thrds; story; na; sc;})

    let reify = reify (TLS.reify) (MemStory.reify) (ViewFront.reify) (ViewFront.reify)

    let init ~regs ~mem =
      let mem   = List.map (fun (l, v) -> (Loc.loc l, Value.integer v)) mem in
      let regs  = List.map Register.reg regs in
      let locs  = List.map fst mem in
      let thrd  = ThreadFront.allocate regs locs in
      let thrds = TLS.leaf thrd in
      let story = MemStory.init mem in
      let na    = ViewFront.allocate locs in
      let sc    = ViewFront.allocate locs in
      state thrds story na sc

    let pprint =
      let pp ff {T.thrds = thrds; T.story = story; T.na = na; T.sc = sc;} =
        Format.fprintf ff "@[<v>%a@;%a@;@[<v>NA-front :@;<1 4>%a@;@]@;@[<v>SC-front :@;<1 4>%a@;@]@]"
          TLS.pprint thrds
          MemStory.pprint story
          ViewFront.pprint na
          ViewFront.pprint sc
      in
      pprint_logic pp

    let get_thrdo t thrdId thrd =
      fresh (tree story na sc)
        (t === state tree story na sc)
        (TLS.geto tree thrdId thrd)

    let set_thrdo t t' thrdId thrd =
      fresh (tree tree' story na sc)
        (t  === state tree  story na sc)
        (t' === state tree' story na sc)
        (TLS.seto tree tree' thrdId thrd)

    let regwriteo t t' thrdId var value =
      fresh (thrd thrd')
        (get_thrdo t    thrdId thrd )
        (set_thrdo t t' thrdId thrd')
        (ThreadFront.writeo thrd thrd' var value)

    let spawno t t' thrdId =
      fresh (tree tree' story na sc)
        (t  === state tree  story na sc)
        (t' === state tree' story na sc)
        (TLS.spawno tree tree' thrdId)

    let joino t t' thrdId =
      fresh (tree tree' story na sc)
        (t  === state tree  story na sc)
        (t' === state tree' story na sc)
        (TLS.joino tree tree' thrdId)

    let returno t t' thrdId rs =
      let rec helper thrd pthrd pthrd'' regs = conde [
        (regs === Std.nil ()) &&& (pthrd === pthrd'');

        fresh (r rs pthrd' v)
          (regs === Std.List.conso r rs)
          (ThreadFront.reado  thrd r v)
          (ThreadFront.writeo pthrd pthrd' r v)
          (helper thrd pthrd' pthrd'' rs);
      ] in
      fresh (tree story na sc parentThrdId thrd pthrd pthrd')
        (t  === state tree  story na sc)
        (ThreadID.parento parentThrdId thrdId)
        (get_thrdo t    thrdId       thrd  )
        (get_thrdo t    parentThrdId pthrd )
        (set_thrdo t t' parentThrdId pthrd')
        (helper thrd pthrd pthrd' rs)

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

    let load_nao t t' thrdId loc value =
      fresh (tree story na sc thrd ts vf)
        (t === t')
        (t === state tree story na sc)
        (TLS.geto tree thrdId thrd)
        (ThreadFront.tso thrd loc ts)
        (MemStory.last_tso story loc ts)
        (na_awareo na loc ts)
        (MemStory.loado story loc ts ts value vf)

    let store_nao t t' thrdId loc value =
      fresh (tree tree' story story' na na' sc thrd thrd' ts ts' rel vf)
        (t  === state tree  story  na  sc)
        (t' === state tree' story' na' sc)
        (TLS.geto tree       thrdId thrd)
        (TLS.seto tree tree' thrdId thrd')
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

    let na_dataraceo t t' thrdId loc =
      fresh (tree story na sc thrd ts)
        (t === t')
        (t === state tree story na sc)
        (TLS.geto tree thrdId thrd)
        (ThreadFront.tso thrd loc ts)
        (conde [
          (na_stucko na loc ts);
          (not_last_tso story loc ts);
        ])

    let dataraceo t t' thrdId loc =
      fresh (tree story na sc thrd ts)
        (t === t')
        (t === state tree story na sc)
        (TLS.geto tree thrdId thrd)
        (ThreadFront.tso thrd loc ts)
        (na_stucko na loc ts)

    let load_rlxo t t' thrdId loc value ts =
      fresh (tree tree' story story' na sc thrd thrd' thrd'' last_ts vf)
        (t  === state tree  story na sc)
        (t' === state tree' story na sc)
        (TLS.geto tree       thrdId thrd)
        (TLS.seto tree tree' thrdId thrd'')
        (ThreadFront.tso thrd loc last_ts)
        (na_awareo na loc last_ts)
        (MemStory.loado story loc last_ts ts value vf)
        (ThreadFront.update_acqo thrd thrd' vf)
        (ThreadFront.updateo thrd' thrd'' loc ts)

    let store_rlxo t t' thrdId loc value ts =
      fresh (tree tree' story story' na sc thrd thrd' last_ts rel)
        (t  === state tree  story  na sc)
        (t' === state tree' story' na sc)
        (TLS.geto tree       thrdId thrd)
        (TLS.seto tree tree' thrdId thrd')
        (ThreadFront.tso thrd loc last_ts)
        (na_awareo na loc last_ts)
        (MemStory.next_tso story loc ts)
        (ThreadFront.front_relo thrd loc rel)
        (ThreadFront.updateo thrd thrd' loc ts)
        (MemStory.storeo story story' loc value rel)

    let fence_acqo t t' thrdId =
      fresh (tree tree' story thrd thrd' na sc)
        (t  === state  tree  story na sc)
        (t' === state  tree' story na sc)
        (TLS.geto tree       thrdId thrd )
        (TLS.seto tree tree' thrdId thrd')
        (ThreadFront.fence_acqo  thrd thrd')

    let fence_relo ?loc t t' thrdId =
      fresh (tree tree' story na sc thrd thrd')
        (t  === state  tree  story na sc)
        (t' === state  tree' story na sc)
        (TLS.geto tree       thrdId thrd )
        (TLS.seto tree tree' thrdId thrd')
        (ThreadFront.fence_relo  thrd thrd' ?loc)

    let load_acqo t t'' thrdId loc value ts =
      fresh (t')
        (load_rlxo t t' thrdId loc value ts)
        (fence_acqo t' t'' thrdId)

    let store_relo t t'' thrdId loc value ts =
      fresh (t')
        (fence_relo t t' thrdId ~loc)
        (store_rlxo t' t'' thrdId loc value ts)

    let load_sco t t' thrdId loc value ts = Timestamp.(
      fresh (tree story na sc sc_ts ts)
        (t === state tree story na sc)
        (load_acqo t t' thrdId loc value ts)
        (ViewFront.tso sc loc sc_ts)
        (sc_ts <= ts)
    )

    let store_sco t t'' thrdId loc value ts =
      fresh (t' tree story na sc sc' ts)
        (t'   === state tree story na sc )
        (t''  === state tree story na sc')
        (store_relo t t' thrdId loc value ts)
        (ViewFront.updateo sc sc' loc ts)

    let load_rlxo t t' thrdId loc value =
      fresh (ts)
        (load_rlxo t t' thrdId loc value ts)

    let store_rlxo t t' thrdId loc value =
      fresh (ts)
        (store_rlxo t t' thrdId loc value ts)

    let load_acqo t t' thrdId loc value =
      fresh (ts)
        (load_acqo t t' thrdId loc value ts)

    let store_relo t t' thrdId loc value =
      fresh (ts)
        (store_relo t t' thrdId loc value ts)

    let load_sco t t' thrdId loc value =
      fresh (ts)
        (load_sco t t' thrdId loc value ts)

    let store_sco t t' thrdId loc value =
      fresh (ts)
        (store_sco t t' thrdId loc value ts)

    let cas_sco t t' thrdId loc expected desired value = success

    let get_regso t thrdId rs =
      fresh (thrd)
        (get_thrdo t thrdId thrd)
        (ThreadFront.get_regso thrd rs)

    let set_regso t t' thrdId rs =
      fresh (thrd thrd')
        (get_thrdo t    thrdId thrd )
        (set_thrdo t t' thrdId thrd')
        (ThreadFront.set_regso thrd thrd' rs)

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

    let transitiono label t t' = conde [
      (label === Label.empty ()) &&& (t === t');

      fresh (thrdId)
        (label === Label.spawn thrdId)
        (spawno t t' thrdId);

      fresh (thrdId)
        (label === Label.join thrdId)
        (joino t t' thrdId);

      fresh (thrdId rs)
        (label === Label.return thrdId rs)
        (returno t t' thrdId rs);

      fresh (thrdId reg v)
        (label === Label.regwrite thrdId reg v)
        (regwriteo t t' thrdId reg v);

      fresh (thrdId mo loc v)
        (label === Label.load thrdId mo loc v)
        (conde [
          (* (mo === !!MemOrder.SC ) &&& (load_sco  t'' t' thrdId loc v); *)
          (mo === !!MemOrder.ACQ) &&& (load_acqo t t' thrdId loc v);
          (mo === !!MemOrder.RLX) &&& (load_rlxo t t' thrdId loc v);
          (mo === !!MemOrder.NA ) &&& (load_nao  t t' thrdId loc v);
        ]);

      fresh (thrdId mo loc v)
        (label === Label.store thrdId mo loc v)
        (conde [
          (* (mo === !!MemOrder.SC ) &&& (store_sco  t t' thrdId loc v); *)
          (mo === !!MemOrder.REL) &&& (store_relo t t' thrdId loc v);
          (mo === !!MemOrder.RLX) &&& (store_rlxo t t' thrdId loc v);
          (mo === !!MemOrder.NA ) &&& (store_nao  t t' thrdId loc v);
        ]);

      fresh (thrdId loc mo v)
        (label === Label.datarace thrdId mo loc)
        (conde [
          (conde [
            (mo === !!MemOrder.SC);
            (mo === !!MemOrder.ACQ);
            (mo === !!MemOrder.REL);
            (mo === !!MemOrder.RLX);
          ]) &&&
          (dataraceo t t' thrdId loc);

          (mo === !!MemOrder.NA) &&&
          (na_dataraceo t t' thrdId loc);
        ]);
    ]
  end

module ReleaseAcquire = Make(MemoryRA)
