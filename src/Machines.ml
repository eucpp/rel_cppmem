open MiniKanren
open Memory
open Utils

module type Sequential =
  sig
    include Utils.Logic

    val reado  : ti ->       Lang.ThreadID.ti -> Lang.Register.ti -> Lang.Value.ti -> MiniKanren.goal
    val writeo : ti -> ti -> Lang.ThreadID.ti -> Lang.Register.ti -> Lang.Value.ti -> MiniKanren.goal
  end

module type Parallel =
  sig
    include Sequential

    val spawno : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal
    val joino  : ti -> ti -> Lang.ThreadID.ti -> MiniKanren.goal
  end

module type NonAtomic =
  sig
    include Parallel

    val load_nao  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val store_nao : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val load_data_raceo  : ti -> ti -> Lang.ThreadID.ti -> Lang.MemOrder.ti -> Lang.Loc.ti -> MiniKanren.goal
    val store_data_raceo : ti -> ti -> Lang.ThreadID.ti -> Lang.MemOrder.ti -> Lang.Loc.ti -> MiniKanren.goal
  end

module type SequentialConsistent =
  sig
    include Parallel

    val load_sco  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val store_sco : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
  end

module type ReleaseAcquire =
  sig
    include Parallel

    val load_acqo  : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
    val store_relo : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
  end

  module GlobalStore =
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

      include Fmap2(T)

      let mem_state thrds sc = inj @@ distrib @@ T.({thrds; sc;})

      let reify = reify (TLS.reify) (ValueStorage.reify)

      let inj x =
        to_logic @@ T.fmap (TLS.inj) (ValueStorage.inj) x

      let preallocate vars atomics =
        let thrd  = RegisterStorage.allocate vars in
        let thrds = TLS.leaf thrd in
        let sc    = ValueStorage.allocate atomics in
        mem_state thrds sc

      let pprint =
        let pp ff {T.thrds = thrds; T.sc = sc;} =
          Format.fprintf ff "@[<v>%a@;@[<v>Memory :@;<1 4>%a@;@]@]"
            TLS.pprint thrds
            ValueStorage.pprint sc
        in
        pprint_logic pp

      let get_thrdo t thrdId thrd =
        fresh (tree sc)
          (t === mem_state tree sc)
          (TLS.geto tree thrdId thrd)

      let set_thrdo t t' thrdId thrd =
        fresh (tree tree' sc)
          (t  === mem_state tree  sc)
          (t' === mem_state tree' sc)
          (TLS.seto tree tree' thrdId thrd)

      let reado t thrdId var value =
        fresh (thrd)
          (get_thrdo t thrdId thrd)
          (RegisterStorage.reado thrd var value)

      let writeo t t' thrdId var value =
        fresh (thrd thrd')
          (get_thrdo t    thrdId thrd )
          (set_thrdo t t' thrdId thrd')
          (RegisterStorage.writeo thrd thrd' var value)

      let load_sco t t' thrdId loc value =
        fresh (tree sc)
          (t === t')
          (t === mem_state tree sc)
          (ValueStorage.reado sc loc value)

      let store_sco t t' thrdId loc value =
        fresh (tree sc sc')
          (t  === mem_state tree sc )
          (t' === mem_state tree sc')
          (ValueStorage.writeo sc sc' loc value)

      let spawno t t' thrdId =
        fresh (tree tree' sc)
          (t  === mem_state tree  sc)
          (t' === mem_state tree' sc)
          (TLS.spawno tree tree' thrdId)

      let joino t t' thrdId =
        fresh (tree tree' sc)
          (t  === mem_state tree  sc)
          (t' === mem_state tree' sc)
          (TLS.joino tree tree' thrdId)
    end

module Front =
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

    include Fmap4(T)

    let mem_state thrds story na sc = inj @@ distrib @@ T.({thrds; story; na; sc;})

    let reify = reify (TLS.reify) (MemStory.reify) (ViewFront.reify) (ViewFront.reify)

    let inj x =
      to_logic @@ T.fmap (TLS.inj) (MemStory.inj) (ViewFront.inj) (ViewFront.inj) x

    (* let to_logic {T.thrds = thrds; T.story = story; T.na = na; T.sc = sc} =
      Value {
        T.thrds = ThreadLocalStorage.to_logic thrds;
        T.story = MemStory.to_logic story;
        T.na    = ViewFront.to_logic na;
        T.sc    = ViewFront.to_logic sc;
      } *)

    (* let refine rr = rr#refine (reify ThreadLocalStorage.reify MemStory.reify ViewFront.reify ViewFront.reify) ~inj:to_logic *)

    (* let create ?(na=ViewFront.from_list []) ?(sc=ViewFront.from_list []) thrds story = {
      T.thrds = thrds;
      T.story = story;
      T.na    = na;
      T.sc    = sc;
    } *)

    let preallocate vars atomics =
      let thrd  = ThreadFront.preallocate vars atomics in
      let thrds = TLS.leaf thrd in
      let story = MemStory.preallocate atomics in
      let na    = ViewFront.allocate atomics in
      let sc    = ViewFront.allocate atomics in
      mem_state thrds story na sc

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
        (t === mem_state tree story na sc)
        (TLS.geto tree thrdId thrd)

    let set_thrdo t t' thrdId thrd =
      fresh (tree tree' story na sc)
        (t  === mem_state tree  story na sc)
        (t' === mem_state tree' story na sc)
        (TLS.seto tree tree' thrdId thrd)

    let reado t thrdId var value =
      fresh (thrd)
        (get_thrdo t thrdId thrd)
        (ThreadFront.reado thrd var value)

    let writeo t t' thrdId var value =
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
        (t === mem_state tree story na sc)
        (TLS.geto tree thrdId thrd)
        (ThreadFront.tso thrd loc ts)
        (MemStory.last_tso story loc ts)
        (na_awareo na loc ts)
        (MemStory.loado story loc ts ts value vf)

    let store_nao t t' thrdId loc value =
      fresh (tree tree' story story' na na' sc thrd thrd' ts ts' rel vf)
        (t  === mem_state tree  story  na  sc)
        (t' === mem_state tree' story' na' sc)
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

    let data_raceo t t' thrdId mo loc =
      fresh (tree story na sc thrd ts)
        (t === t')
        (t === mem_state tree story na sc)
        (TLS.geto tree thrdId thrd)
        (ThreadFront.tso thrd loc ts)
        (conde [
          (mo === !!Lang.MemOrder.NA ) &&& ((na_stucko na loc ts) ||| (not_last_tso story loc ts));
          (* (mo === !!Lang.MemOrder.ACQ) &&& (na_stucko na loc ts); *)
          (* (mo === !!Lang.MemOrder.SC ) &&& (na_stucko na loc ts); *)
        ])

    let load_data_raceo  = data_raceo
    let store_data_raceo = data_raceo

    let loado t t' thrdId loc value ts =
      fresh (tree tree' story story' na sc thrd thrd' thrd'' last_ts vf)
        (t  === mem_state tree  story na sc)
        (t' === mem_state tree' story na sc)
        (TLS.geto tree       thrdId thrd)
        (TLS.seto tree tree' thrdId thrd'')
        (ThreadFront.tso thrd loc last_ts)
        (na_awareo na loc last_ts)
        (MemStory.loado story loc last_ts ts value vf)
        (ThreadFront.update_acqo thrd thrd' vf)
        (ThreadFront.updateo thrd' thrd'' loc ts)

    let storeo t t' thrdId loc value ts =
      fresh (tree tree' story story' na sc thrd thrd' last_ts rel)
        (t  === mem_state tree  story  na sc)
        (t' === mem_state tree' story' na sc)
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
        (t  === mem_state  tree  story na sc)
        (t' === mem_state  tree' story na sc)
        (TLS.geto tree       thrdId thrd )
        (TLS.seto tree tree' thrdId thrd')
        (ThreadFront.fence_acqo  thrd thrd')

    let fence_relo ?loc t t' thrdId =
      fresh (tree tree' story na sc thrd thrd')
        (t  === mem_state  tree  story na sc)
        (t' === mem_state  tree' story na sc)
        (TLS.geto tree       thrdId thrd )
        (TLS.seto tree tree' thrdId thrd')
        (ThreadFront.fence_relo  thrd thrd' ?loc)

    let load_acqo t t'' thrdId loc value ts =
      fresh (t')
        (loado t t' thrdId loc value ts)
        (fence_acqo t' t'' thrdId)

    let store_relo t t'' thrdId loc value ts =
      fresh (t')
        (fence_relo t t' thrdId ~loc)
        (storeo t' t'' thrdId loc value ts)

    let load_sco t t' thrdId loc value ts = Timestamp.(
      fresh (tree story na sc sc_ts ts)
        (t === mem_state tree story na sc)
        (load_acqo t t' thrdId loc value ts)
        (ViewFront.tso sc loc sc_ts)
        (sc_ts <= ts)
    )

    (* let read_sco t t' thrdId loc value ts = Nat.(
      fresh (tree story na sc)
        (t === t')
        (t === mem_state tree story na sc)
        (VarList.geto sc loc value)
    ) *)

    let store_sco t t'' thrdId loc value ts =
      fresh (t' tree story na sc sc' ts)
        (t'   === mem_state tree story na sc )
        (t''  === mem_state tree story na sc')
        (store_relo t t' thrdId loc value ts)
        (ViewFront.updateo sc sc' loc ts)

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

    (* let write_sco t t' thrdId loc value ts =
      fresh (tree story na sc sc')
        (t  === mem_state tree story na sc )
        (t' === mem_state tree story na sc')
        (VarList.seto sc sc' loc value) *)

    let last_valueo t loc value =
      fresh (tree tree story na sc)
        (t  === mem_state tree story na sc)
        (MemStory.last_valueo story loc value)

    (* let promiseo t t' thrdId loc value =
      fresh (tree tree' story story' na sc thrd thrd' ts rel)
        (t  === mem_state  tree  story  na sc)
        (t' === mem_state  tree' story' na sc)
        (ThreadLocalStorage.geto tree       thrdId thrd )
        (ThreadLocalStorage.seto tree tree' thrdId thrd')
        (MemStory.next_tso story loc ts)
        (ThreadFront.front_relo thrd loc rel)
        (ThreadFront.promiseo thrd thrd' loc ts value rel)
        (MemStory.writeo story story' loc value rel)

    let fulfillo t t' thrdId =
      fresh (tree tree' story na sc thrd thrd' ts)
        (t  === mem_state  tree  story na sc)
        (t' === mem_state  tree' story na sc)
        (ThreadLocalStorage.geto tree       thrdId thrd )
        (ThreadLocalStorage.seto tree tree' thrdId thrd')
        (ThreadFront.fulfillo thrd thrd')

    let laggingo t b =
      fresh (tree story na sc)
        (t === mem_state tree story na sc)
        (ThreadLocalStorage.laggingo tree b)

    let certifyo t thrdId =
      fresh (tree story na sc thrd)
        (t  === mem_state tree story na sc)
        (ThreadLocalStorage.geto tree thrdId thrd)
        (ThreadFront.certifyo thrd) *)

    let spawno t t' thrdId =
      fresh (tree tree' story na sc)
        (t  === mem_state tree  story na sc)
        (t' === mem_state tree' story na sc)
        (TLS.spawno tree tree' thrdId)

    let joino t t' thrdId =
      fresh (tree tree' story na sc)
        (t  === mem_state tree  story na sc)
        (t' === mem_state tree' story na sc)
        (TLS.joino tree tree' thrdId)
  end
