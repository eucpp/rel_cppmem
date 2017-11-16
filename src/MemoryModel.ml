open MiniKanren
open Memory
open Lang
open Utils

module type State =
  sig
    include Utils.Logic

    val init : regs:Lang.Register.ti list -> locs:Lang.Loc.ti list -> ti

    val regso : ti -> Lang.ThreadID.ti -> Memory.RegisterStorage.ti -> MiniKanren.goal

    val transitiono : Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end

module Make (S : State) =
  struct
    module Node = Semantics.MakeConfig(Lang.Term)(S)

    let ctx_lifto ctx' state ctx =
      fresh (rs thrdId)
        (ctx === Rules.Context.context ctx' rs)
        (Lang.Context.thrdIdo ctx' thrdId)
        (S.regso state thrdId rs)

    let lift_split splito term ctx rdx =
      fresh (term' ctx' rdx' state)
        (term === Node.cfg term' state)
        (rdx  === Node.cfg rdx'  state)
        (splito term' ctx' rdx')
        (ctx_lifto ctx' state ctx)

    let lift_plug plugo ctx rdx term =
      fresh (term' ctx' rdx' state)
        (term === Node.cfg term' state)
        (rdx  === Node.cfg rdx'  state)
        (plugo ctx' rdx' term')
        (ctx_lifto ctx' state ctx)

    let lift_rule rule ctx t t' =
      fresh (label prog prog' state state')
        (t  === Node.cfg prog  state )
        (t' === Node.cfg prog' state')
        (rule label ctx prog prog')
        (S.transitiono label state state')

    let thrd_local_stepo thrdId = Semantics.Reduction.make_step
      (lift_split @@ thrd_splito thrdId)
      (lift_plug Lang.plugo)
      (List.map lift_rule Rules.Basic.all)

    let rec thrd_local_evalo thrdId =
      let irreducibleo = Node.lift_tpred @@ fun term -> conde [
        (term === Lang.Term.stuck ());

        fresh (ctx rdx)
          (term =/= Lang.Term.stuck ())
          (Lang.thrd_splito thrdId term ctx rdx)
          (conde [
            (Lang.Term.irreducibleo rdx);
            (Lang.Term.thrd_inter_termo rdx);
          ]);
      ] in
      Semantics.Reduction.make_eval ~irreducibleo (thrd_local_stepo thrdId)

  let thrd_inter_stepo thrdId = Semantics.Reduction.make_step
    (lift_split @@ thrd_splito thrdId)
    (lift_plug Lang.plugo)
    (List.map lift_rule (Rules.ThreadSpawning.all @ Rules.Atomic.all))

  let stepo t t'' =
    fresh (thrdId t')
      (thrd_local_evalo thrdId t t')
      (conde [
        fresh (thrdId')
          (t === t')
          (thrd_inter_stepo thrdId' t' t'');

        (t =/= t') &&& (conde [
          (t' === t'');
          (thrd_inter_stepo thrdId t' t'');
        ]);
      ])

  let evalo = Semantics.Reduction.make_eval ~irreducibleo:(Node.lift_tpred Lang.Term.irreducibleo) stepo

  end

(* module SequentialConsistent =
  struct
    module State =
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

        let inj x =
          to_logic @@ T.fmap (TLS.inj) (ValueStorage.inj) x

        let init ~regs ~locs =
          let thrd  = RegisterStorage.allocate regs in
          let thrds = TLS.leaf thrd in
          let sc    = ValueStorage.allocate locs in
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

        let regreado t t' thrdId var value =
          fresh (thrd)
            (t === t')
            (get_thrdo t thrdId thrd)
            (RegisterStorage.reado thrd var value)

        let regwriteo t t' thrdId var value =
          fresh (thrd thrd')
            (get_thrdo t    thrdId thrd )
            (set_thrdo t t' thrdId thrd')
            (RegisterStorage.writeo thrd thrd' var value)

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
          (Lang.Value.eqo value expected b)
          (conde [
            (b === !!true)  &&& (ValueStorage.writeo sc sc' loc desired);
            (b === !!false) &&& (sc === sc');
          ])

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

        let transitiono label t t' = conde [
          (label === Label.empty ()) &&& (t === t');

          fresh (thrdId)
            (label === Label.spawn thrdId)
            (spawno t t' thrdId);

          fresh (thrdId)
            (label === Label.join thrdId)
            (joino t t' thrdId);

          fresh (thrdId reg v)
            (label === Label.regread thrdId reg v)
            (regreado t t' thrdId reg v);

          fresh (thrdId reg v)
            (label === Label.regwrite thrdId reg v)
            (regwriteo t t' thrdId reg v);

          fresh (thrdId loc v)
            (label === Label.load thrdId !!MemOrder.SC loc v)
            (load_sco t t' thrdId loc v);

          fresh (thrdId loc v)
            (label === Label.store thrdId !!MemOrder.SC loc v)
            (store_sco t t' thrdId loc v);

          fresh (thrdId loc e d v)
            (label === Label.cas thrdId !!MemOrder.SC !!MemOrder.SC loc e d v)
            (cas_sco t t' thrdId loc e d v);
        ]
      end

    module TLSNode = Semantics.TLSNode(Lang.Term)(State)

    let thrd_local_splito thrdId term result =
      fresh (result')
        (thrd_splito thrdId term result')
        (conde [
          fresh (ctx rdx)
            (result' === Semantics.Split.split ctx rdx)
            (conde [
              (result === result') &&& (Lang.Term.thrd_local_termo rdx);

              (result === Semantics.Split.undef ()) &&& (Lang.Term.thrd_inter_termo rdx);
            ]);

          (result' === Semantics.Split.undef ());
        ])

    let thrd_local_stepo thrdId = Semantics.make_step
      (TLSNode.lift_split @@ thrd_local_splito thrdId)
      (TLSNode.lift_plug Lang.plugo)
      (List.map (fun rule -> TLSNode.lift_rule rule) Rules.Basic.all)

    let thrd_local_evalo thrdId = Semantics.make_eval @@ thrd_local_stepo thrdId

    let thrd_inter_splito thrdId term result =
      fresh (result')
        (thrd_splito thrdId term result')
        (conde [
          fresh (ctx rdx)
            (result' === Semantics.Split.split ctx rdx)
            (conde [
              (result === Semantics.Split.undef ()) &&& (Lang.Term.thrd_local_termo rdx);

              (result === result') &&& (Lang.Term.thrd_inter_termo rdx);
            ]);

          (result' === Semantics.Split.undef ());
        ])

    let thrd_inter_stepo thrdId = Semantics.make_step
      (TLSNode.lift_split @@ thrd_inter_splito thrdId)
      (TLSNode.lift_plug Lang.plugo)
      (List.map (fun rule -> TLSNode.lift_rule rule) (Rules.ThreadSpawning.all @ Rules.Atomic.all))

    let stepo t result =
      fresh (thrdId t')
        (thrd_local_evalo thrdId t t')
        (thrd_inter_stepo thrdId t' result)

    let evalo = Semantics.make_eval stepo
  end *)

module ReleaseAcquire =
  struct
    module State =
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

        let init ~regs ~locs =
          let thrd  = ThreadFront.preallocate regs locs in
          let thrds = TLS.leaf thrd in
          let story = MemStory.preallocate locs in
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

        let regreado t t' thrdId var value =
          fresh (thrd)
            (t === t')
            (get_thrdo t thrdId thrd)
            (ThreadFront.reado thrd var value)

        let regwriteo t t' thrdId var value =
          fresh (thrd thrd')
            (get_thrdo t    thrdId thrd )
            (set_thrdo t t' thrdId thrd')
            (ThreadFront.writeo thrd thrd' var value)

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
            (MemStory.last_tso story loc last_ts)
            (ts =/= last_ts)

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

            let get_thrdo t thrdId thrd =
              fresh (tree story na sc)
                (t === state tree story na sc)
                (TLS.geto tree thrdId thrd)

        let regso t thrdId rs =
          fresh (thrd)
            (get_thrdo t thrdId thrd)
            (ThreadFront.regso thrd rs)

        let transitiono label t t' = conde [
          (label === Label.empty ()) &&& (t === t');

          fresh (thrdId)
            (label === Label.spawn thrdId)
            (spawno t t' thrdId);

          fresh (thrdId)
            (label === Label.join thrdId)
            (joino t t' thrdId);

          fresh (thrdId reg v)
            (label === Label.regread thrdId reg v)
            (regreado t t' thrdId reg v);

          fresh (thrdId reg v)
            (label === Label.regwrite thrdId reg v)
            (regwriteo t t' thrdId reg v);

          fresh (t'' thrdId loc reg v)
            (label === Label.load thrdId !!MemOrder.SC loc reg)
            (load_sco  t   t'' thrdId loc v)
            (regwriteo t'' t'  thrdId reg v);

          fresh (thrdId loc v)
            (label === Label.store thrdId !!MemOrder.SC loc v)
            (store_sco t t' thrdId loc v);

          fresh (t'' thrdId loc reg v)
            (label === Label.load thrdId !!MemOrder.ACQ loc reg)
            (load_acqo t   t'' thrdId loc v)
            (regwriteo t'' t'  thrdId reg v);

          fresh (thrdId loc v)
            (label === Label.store thrdId !!MemOrder.REL loc v)
            (store_relo t t' thrdId loc v);

          fresh (t'' thrdId loc reg v)
            (label === Label.load thrdId !!MemOrder.RLX loc reg)
            (load_rlxo t   t'' thrdId loc v)
            (regwriteo t'' t'  thrdId reg v);

          fresh (thrdId loc v)
            (label === Label.store thrdId !!MemOrder.RLX loc v)
            (store_rlxo t t' thrdId loc v);

          fresh (t'' thrdId loc reg v)
            (label === Label.load thrdId !!MemOrder.NA loc reg)
            (load_nao  t   t'' thrdId loc v)
            (regwriteo t'' t'  thrdId reg v);

          fresh (thrdId loc v)
            (label === Label.store thrdId !!MemOrder.NA loc v)
            (store_nao t t' thrdId loc v);

          fresh (thrdId loc mo v)
            (label === Label.datarace thrdId mo loc)
            (conde [
              (conde [
                (mo === !!MemOrder.SC);
                (mo === !!MemOrder.ACQ);
                (mo === !!MemOrder.REL);
              ]) &&&
              (dataraceo t t' thrdId loc);

              (mo === !!MemOrder.NA) &&&
              (na_dataraceo t t' thrdId loc);
            ]);
        ]
      end

    include Make(State)

  end
