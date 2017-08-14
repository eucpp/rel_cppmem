open Memory

module type Sequential =
  sig
    type tt

    type tl = inner MiniKanren.logic
      and inner

    type ti = (tt, tl) MiniKanren.injected

    val reado  : ti ->       Lang.ThreadID.ti -> Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal
    val writeo : ti -> ti -> Lang.ThreadID.ti -> Lang.Var.ti -> Lang.Value.ti -> MiniKanren.goal
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

    val data_raceo : ti -> ti -> Lang.ThreadID.ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal
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

    type tt = (
      ThreadFront.tt ThreadLocalStorage.tt,
      MemStory.tt,
      ViewFront.tt,
      ViewFront.tt
    ) T.t

    type tl = inner MiniKanren.logic
      and inner = (
        ThreadFront.tl ThreadLocalStorage.tl,
        MemStory.tl,
        ViewFront.tl,
        ViewFront.tl
      ) T.t

    type ti = (tt, tl) MiniKanren.injected

    include Fmap4(T)

    let mem_state thrds story na sc = inj @@ distrib @@ T.({thrds; story; na; sc;})

    let inj {T.thrds = thrds; T.story = story; T.na = na; T.sc = sc;} =
      mem_state (ThreadLocalStorage.inj thrds) (MemStory.inj story) (ViewFront.inj na) (ViewFront.inj sc)

    (* let to_logic {T.thrds = thrds; T.story = story; T.na = na; T.sc = sc} =
      Value {
        T.thrds = ThreadLocalStorage.to_logic thrds;
        T.story = MemStory.to_logic story;
        T.na    = ViewFront.to_logic na;
        T.sc    = ViewFront.to_logic sc;
      } *)

    let refine rr = rr#refine (reify ThreadLocalStorage.reify MemStory.reify ViewFront.reify ViewFront.reify) ~inj:to_logic

    (* let create ?(na=ViewFront.from_list []) ?(sc=ViewFront.from_list []) thrds story = {
      T.thrds = thrds;
      T.story = story;
      T.na    = na;
      T.sc    = sc;
    } *)

    let preallocate vars atomics =
      let thrd  = ThreadFront.preallocate vars atomics in
      let thrds = ThreadLocalStorage.leaf thrd in
      let story = MemStory.preallocate atomics in
      let na    = ViewFront.allocate atomics in
      let sc    = ViewFront.allocate atomics in
      create thrds story ~na ~sc

    let printer =
      let pp ff {T.thrds = thrds; T.story = story; T.na = na; T.sc = sc;} =
        Format.fprintf ff "@[<v>%a@;%a@;@[<v>NA-front :@;<1 4>%a@;@]@;@[<v>SC-front :@;<1 4>%a@;@]@]"
          ThreadLocalStorage.printer thrds
          MemStory.printer story
          ViewFront.printer na
          ViewFront.printer sc
      in
      pprint_logic pp

    let get_thrdo t thrdId thrd =
      fresh (tree story na sc)
        (t === mem_state tree story na sc)
        (ThreadLocalStorage.geto tree thrdId thrd)

    let set_thrdo t t' thrdId thrd =
      fresh (tree tree' story na sc)
        (t  === mem_state tree  story na sc)
        (t' === mem_state tree' story na sc)
        (ThreadLocalStorage.seto tree tree' path thrd)

    let reado t thrdId var value =
      fresh (thrd)
        (get_thrdo t thrdId thrd)
        (ThreadFront.get_varo thrd var value)

    let writeo t t' thrdId var value =
      fresh (thrd thrd')
        (get_thrdo t    thrdId thrd )
        (set_thrdo t t' thrdId thrd')
        (ThreadFront.set_varo thrd thrd' var value)

    let na_awareo na loc last_ts = Nat.(
      fresh (ts na_ts)
        (ViewFront.tso na loc na_ts)
        (na_ts <= last_ts)
    )

    let load_nao t t' thrdId loc value =
      fresh (tree story na sc thrd ts vf)
        (t === t')
        (t === mem_state tree story na sc)
        (ThreadLocalStorage.geto tree thrdId thrd)
        (ThreadFront.tso thrd loc ts)
        (MemStory.last_tso story loc ts)
        (na_awareo na loc ts)
        (MemStory.loado story loc ts ts value vf)

    let store_nao t t' path loc value =
      fresh (tree tree' story story' na na' sc thrd thrd' ts ts' rel vf)
        (t  === mem_state tree  story  na  sc)
        (t' === mem_state tree' story' na' sc)
        (ThreadLocalStorage.geto tree       path thrd)
        (ThreadLocalStorage.seto tree tree' path thrd')
        (ThreadFront.tso thrd loc ts)
        (MemStory.last_tso story loc ts)
        (na_awareo na loc ts)

        (MemStory.next_tso story loc ts')
        (ThreadFront.updateo thrd thrd' loc ts')
        (ViewFront.updateo na na' loc ts')
        (MemStory.writeo story story' loc value (ViewFront.bottom ()))

    let na_stucko t path loc =
      fresh (tree story na sc thrd ts last_ts)
        (t === mem_state tree story na sc)
        (ThreadLocalStorage.geto tree path thrd)
        (ThreadFront.tso thrd loc ts)
        (MemStory.last_tso story loc last_ts)
        (ts =/= last_ts)

    let read_na_dro t t' path loc =
      (t === t') &&&
      (na_stucko t path loc)

    let write_na_dro t t' path loc =
      (t === t') &&&
      (na_stucko t path loc)

    let data_raceo t path loc = Nat.(
      fresh (tree story na sc thrd ts write_ts)
        (t === mem_state tree story na sc)
        (ThreadLocalStorage.geto tree path thrd)
        (ThreadFront.tso thrd loc ts)
        (VarList.geto na loc write_ts)
        (ts < write_ts)
    )

    let read_dro t t' path loc =
      (t === t') &&&
      (data_raceo t path loc)

    let write_dro t t' path loc =
      (t === t') &&&
      (data_raceo t path loc)

    let read_rlxo t t' path loc value ts =
      fresh (tree tree' story story' na sc thrd thrd' thrd'' last_ts vf)
        (t  === mem_state tree  story na sc)
        (t' === mem_state tree' story na sc)
        (ThreadLocalStorage.geto tree       path thrd)
        (ThreadLocalStorage.seto tree tree' path thrd'')
        (na_awareo na thrd loc)
        (ThreadFront.tso thrd loc last_ts)
        (MemStory.reado story loc last_ts ts value vf)
        (ThreadFront.update_acqo thrd thrd' vf)
        (ThreadFront.updateo thrd' thrd'' loc ts)

    let write_rlxo t t' path loc value ts =
      fresh (tree tree' story story' na sc thrd thrd' rel)
        (t  === mem_state tree  story  na sc)
        (t' === mem_state tree' story' na sc)
        (ThreadLocalStorage.geto tree       path thrd)
        (ThreadLocalStorage.seto tree tree' path thrd')
        (na_awareo na thrd loc)
        (MemStory.next_tso story loc ts)
        (ThreadFront.front_relo thrd loc rel)
        (ThreadFront.updateo thrd thrd' loc ts)
        (MemStory.writeo story story' loc value rel)

    let fence_acqo t t' path =
      fresh (tree tree' story thrd thrd' na sc)
        (t  === mem_state  tree  story na sc)
        (t' === mem_state  tree' story na sc)
        (ThreadLocalStorage.geto tree       path thrd )
        (ThreadLocalStorage.seto tree tree' path thrd')
        (ThreadFront.fence_acqo  thrd thrd')

    let fence_relo t t' path =
      fresh (tree tree' story na sc thrd thrd')
        (t  === mem_state  tree  story na sc)
        (t' === mem_state  tree' story na sc)
        (ThreadLocalStorage.geto tree       path thrd )
        (ThreadLocalStorage.seto tree tree' path thrd')
        (ThreadFront.fence_relo  thrd thrd')

    let fence_loc_relo t t' path loc =
      fresh (tree tree' story na sc thrd thrd')
        (t  === mem_state  tree  story na sc)
        (t' === mem_state  tree' story na sc)
        (ThreadLocalStorage.geto tree       path thrd )
        (ThreadLocalStorage.seto tree tree' path thrd')
        (ThreadFront.fence_loc_relo thrd thrd' loc)

    let read_acqo t t'' path loc value ts =
      fresh (t')
        (read_rlxo t t' path loc value ts)
        (fence_acqo t' t'' path)

    let write_relo t t'' path loc value ts =
      fresh (t')
        (fence_loc_relo t t' path loc)
        (write_rlxo t' t'' path loc value ts)

    (* let read_sco t t' path loc value ts = Nat.(
      fresh (tree story na sc sc_ts ts)
        (t === mem_state tree story na sc)
        (read_acqo t t' path loc value ts)
        (VarList.geto sc loc sc_ts)
        (sc_ts <= ts)
    ) *)

    let read_sco t t' path loc value ts = Nat.(
      fresh (tree story na sc)
        (t === t')
        (t === mem_state tree story na sc)
        (VarList.geto sc loc value)
    )

    (* let write_sco t t'' path loc value ts =
      fresh (t' tree story na sc sc' ts)
        (t'   === mem_state tree story na sc )
        (t''  === mem_state tree story na sc')
        (write_relo t t' path loc value ts)
        (ViewFront.updateo sc sc' loc ts) *)

    let write_sco t t' path loc value ts =
      fresh (tree story na sc sc')
        (t  === mem_state tree story na sc )
        (t' === mem_state tree story na sc')
        (VarList.seto sc sc' loc value)

    let last_valueo t loc value =
      fresh (tree tree story na sc)
        (t  === mem_state tree story na sc)
        (MemStory.last_valueo story loc value)

    (* let promiseo t t' path loc value =
      fresh (tree tree' story story' na sc thrd thrd' ts rel)
        (t  === mem_state  tree  story  na sc)
        (t' === mem_state  tree' story' na sc)
        (ThreadLocalStorage.geto tree       path thrd )
        (ThreadLocalStorage.seto tree tree' path thrd')
        (MemStory.next_tso story loc ts)
        (ThreadFront.front_relo thrd loc rel)
        (ThreadFront.promiseo thrd thrd' loc ts value rel)
        (MemStory.writeo story story' loc value rel)

    let fulfillo t t' path =
      fresh (tree tree' story na sc thrd thrd' ts)
        (t  === mem_state  tree  story na sc)
        (t' === mem_state  tree' story na sc)
        (ThreadLocalStorage.geto tree       path thrd )
        (ThreadLocalStorage.seto tree tree' path thrd')
        (ThreadFront.fulfillo thrd thrd')

    let laggingo t b =
      fresh (tree story na sc)
        (t === mem_state tree story na sc)
        (ThreadLocalStorage.laggingo tree b)

    let certifyo t path =
      fresh (tree story na sc thrd)
        (t  === mem_state tree story na sc)
        (ThreadLocalStorage.geto tree path thrd)
        (ThreadFront.certifyo thrd) *)

    let spawno t t' path =
      fresh (tree tree' story na sc)
        (t  === mem_state tree  story na sc)
        (t' === mem_state tree' story na sc)
        (ThreadLocalStorage.spawno (ThreadFront.spawno) tree tree' path)

    let joino t t' path =
      fresh (tree tree' story na sc)
        (t  === mem_state tree  story na sc)
        (t' === mem_state tree' story na sc)
        (ThreadLocalStorage.joino (ThreadFront.joino) tree tree' path)
  end
