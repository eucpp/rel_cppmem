open MiniKanren
open MiniKanrenStd
open Utils

module ValueStorage =
  struct
    type tt = (Lang.Loc.tt, Lang.Value.tt) Storage.tt

    type tl = (Lang.Loc.tl, Lang.Value.tl) Storage.tl
      and inner = (Lang.Loc.tl, Lang.Value.tl) Storage.inner

    type ti = (Lang.Loc.tt, Lang.Value.tt, Lang.Loc.tl, Lang.Value.tl) Storage.ti

    let allocate = Storage.allocate (Lang.Value.integer 0)

    let from_assoc = Storage.from_assoc

    let shapeo = Storage.shapeo

    let reify = Storage.reify (Lang.Loc.reify) (Lang.Value.reify)

    let pprint =
      Storage.pprint (fun ff (k, v) -> Format.fprintf ff "%s=%s" (Lang.Loc.show k) (Lang.Value.show v))

    let reado  = Storage.geto
    let writeo = Storage.seto
  end

module Timestamp =
  struct
    type tt = Nat.ground

    type tl = inner MiniKanren.logic
      and inner = tl nat

    type ti = (tt, tl) MiniKanren.injected

    let ts = nat

    let reify = Nat.reify

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
          Printf.sprintf "_.??"
        | Var (i, []) ->
          Printf.sprintf "_.%d" i
        | Var (i, cs) ->
          let cs = String.concat "; " @@ List.map show cs in
          Printf.sprintf "_.%d{=/= %s}" i cs

    let pprint ff ts = Format.fprintf ff "%s" @@ show ts

    let (<)  = Nat.(<)
    let (<=) = Nat.(<=)
    let (>)  = Nat.(>)
    let (>=) = Nat.(>=)
  end

module ViewFront =
  struct
    type tt = (Lang.Loc.tt, Timestamp.tt) Storage.tt

    type tl = (Lang.Loc.tl, Timestamp.tl) Storage.tl
      and inner = (Lang.Loc.tl, Timestamp.tl) Storage.inner

    type ti = (Lang.Loc.tt, Timestamp.tt, Lang.Loc.tl, Timestamp.tl) Storage.ti

    let bottom = Storage.empty

    let allocate = Storage.allocate (Timestamp.ts 0)

    let from_assoc = Storage.from_assoc

    let reify = Storage.reify (Lang.Loc.reify) (Timestamp.reify)

    let pprint =
      Storage.pprint (fun ff (k, v) -> Format.fprintf ff "%s@%s" (Lang.Loc.show k) (Timestamp.show v))

    let shapeo = Storage.shapeo

    let tso = Storage.geto

    let updateo t t' loc ts = Nat.(
      let r ts_old ts_new = conde [
          (ts >  ts_old) &&& (ts_new === ts);
          (ts <= ts_old) &&& (ts_new === ts_old);
      ] in
      Storage.updateo r t t' loc
    )

    let mergeo t1 t2 t' =
      let r l1 ts1 l2 ts2 l' ts' = Nat.(
        (l1 === l2) &&& (l2 === l') &&&
        (conde [
          (ts1 >  ts2) &&& (ts' === ts1);
          (ts1 <= ts2) &&& (ts' === ts2);
        ])
      ) in
      conde [
        (t1 === bottom ()) &&& (t2 === bottom ()) &&& (t' === bottom ());
        (t1 === bottom ()) &&& (t2 =/= bottom ()) &&& (t' === t2);
        (t1 =/= bottom ()) &&& (t2 === bottom ()) &&& (t' === t1);
        (t1 =/= bottom ()) &&& (t2 =/= bottom ()) &&& (Storage.map2o r t1 t2 t');
      ]
  end

module ThreadFront =
  struct
    module T = struct
      type ('curr, 'rel, 'acq) t = {
        curr : 'curr;
        rel  : 'rel;
        acq  : 'acq;
      }

      let fmap f g h {curr; rel; acq} =
        {curr = f curr; rel = g rel; acq = h acq}
    end

    type tt = (ViewFront.tt, ViewFront.tt, ViewFront.tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (ViewFront.tl, ViewFront.tl, ViewFront.tl) T.t

    type ti = (tt, tl) MiniKanren.injected

    include Fmap3(T)

    let thrd_state curr rel acq =
      inj @@ distrib @@ {T.curr = curr; T.rel = rel; T.acq = acq; }

    let reify = reify ViewFront.reify ViewFront.reify ViewFront.reify

    let convert = (fun (var, value) -> (var, Nat.of_int value))

    let allocate atomics = thrd_state
      (ViewFront.allocate atomics)
      (ViewFront.allocate atomics)
      (ViewFront.allocate atomics)

    let pprint =
      let pp ff = let open T in fun { curr; rel; acq } ->
        Format.fprintf ff "@[<v>cur: %a @;acq: %a @;rel: %a @]"
          ViewFront.pprint curr
          ViewFront.pprint acq
          ViewFront.pprint rel
      in
      pprint_logic pp

    let tso thrd loc ts =
      fresh (curr rel acq)
        (thrd === thrd_state curr rel acq)
        (ViewFront.tso curr loc ts)

    let updateo thrd thrd' loc ts =
      fresh (curr curr' rel acq acq')
        (thrd  === thrd_state curr  rel  acq )
        (thrd' === thrd_state curr' rel  acq')
        (ViewFront.updateo curr curr' loc ts)
        (ViewFront.updateo acq  acq'  loc ts)

    let front_relo thrd loc rel =
      fresh (curr acq)
        (thrd === thrd_state curr rel acq)

    let update_acqo thrd thrd' vf =
      fresh (curr rel acq acq')
        (thrd  === thrd_state curr rel acq )
        (thrd' === thrd_state curr rel acq')
        (ViewFront.mergeo vf acq acq')

    let fence_acqo thrd thrd' =
      fresh (curr rel acq)
        (thrd  === thrd_state curr rel acq)
        (thrd' === thrd_state acq  rel acq)

    let fence_relo ?loc thrd thrd' =
      fresh (curr rel acq)
        (thrd  === thrd_state curr rel  acq)
        (thrd' === thrd_state curr curr acq)

    let spawno thrd child1 child2 =
      (thrd === child1) &&& (thrd === child2)

    let joino thrd thrd' child1 child2 =
      fresh (curr curr' curr1 curr2
             rel  rel' rel1 rel2
             acq  acq' acq1 acq2)
        (thrd   === thrd_state curr  rel  acq )
        (thrd'  === thrd_state curr' rel' acq')
        (child1 === thrd_state curr1 rel1 acq1)
        (child2 === thrd_state curr2 rel2 acq2)
        (ViewFront.mergeo curr1 curr2 curr')
        (ViewFront.mergeo rel1  rel2  rel' )
        (ViewFront.mergeo acq1  acq2  acq' )

  end

module type ThreadLocalData =
  sig
    include Utils.Logic

    val spawno : ti -> ti -> ti -> MiniKanren.goal
    val joino  : ti -> ti -> ti -> ti -> MiniKanren.goal
  end

module LocStory =
  struct
    module Cell =
      struct
        module T = struct
          type ('a, 'b, 'c) t = {
            ts  : 'a;
            v   : 'b;
            vf  : 'c;
          }

          let fmap fa fb fc {ts = a; v = b; vf = c} =
            {ts = fa a; v = fb b; vf = fc c}
        end

        type tt = (Timestamp.tt, Lang.Value.tt, ViewFront.tt) T.t

        type tl = inner MiniKanren.logic
          and inner = (Timestamp.tl, Lang.Value.tl, ViewFront.tl) T.t

        type ti = (tt, tl) MiniKanren.injected

        include Fmap3(T)

        let cell ts v vf = T.(inj @@ distrib {ts; v; vf})

        let reify = reify Timestamp.reify Lang.Value.reify ViewFront.reify

        let pprint =
          let open T in
          let pp ff {ts; v; vf} =
            Format.fprintf ff "@[<h>{@%s, %s, %a}@]"
              (Timestamp.show ts)
              (Lang.Value.show v)
              ViewFront.pprint vf
          in
          pprint_logic pp
      end

    module T = struct
      type ('a, 'b) t = {
        tsnext : 'a;
        story  : 'b;
      }

      let fmap fa fb {tsnext = a; story = b} = {tsnext = fa a; story = fb b}
    end

    type tt = (Timestamp.tt, Cell.tt List.ground) T.t

    type tl = inner MiniKanren.logic
      and inner = (Timestamp.tl, Cell.tl List.logic) T.t

    type ti = (tt, tl) MiniKanren.injected

    include Fmap2(T)

    let loc_story tsnext story = inj @@ distrib @@ {T.tsnext = tsnext; T.story = story}

    let allocate () =
      let vf = ViewFront.bottom () in
      inj @@ distrib @@ {
        T.tsnext = Timestamp.ts 1;
        T.story = MiniKanren.Std.List.list [Cell.cell (Timestamp.ts 0) (Lang.Value.integer 0) vf];
      }

    let init v =
      let vf = ViewFront.bottom () in
      inj @@ distrib @@ {
        T.tsnext = Timestamp.ts 1;
        T.story = MiniKanren.Std.List.list [Cell.cell (Timestamp.ts 0) v vf];
      }

    let reify = reify Timestamp.reify (List.reify Cell.reify)

    let create tsnext story = {
      T.tsnext = Nat.of_int tsnext;
      T.story  = List.of_list (fun (ts, v, vf) -> (Nat.of_int ts, Nat.of_int v, vf)) story;
    }

    let pprint =
      let pp ff {T.story = story} =
        pprint_llist Cell.pprint ff story
      in
      pprint_logic pp

    let last_tso t ts =
      fresh (ts' story)
        (t   === loc_story ts' story)
        (ts' === Nat.succ ts)

    let next_tso t ts =
      fresh (story)
        (t === loc_story ts story)

    let visibleo ts msg b =
      fresh (ts' value vf)
        (msg === Cell.cell ts' value vf)
        (Nat.leo ts ts' b)

    let loado t last_ts ts value vf =
      fresh (story tsnext visible msg)
        (t === loc_story tsnext story)
        (msg === Cell.cell ts value vf)
        (MiniKanrenStd.List.filtero (visibleo last_ts) story visible)
        (MiniKanrenStd.List.membero visible msg)

    let storeo t t' value vf =
      fresh (ts ts' story story')
        (t  === loc_story ts  story )
        (t' === loc_story ts' story')
        (ts' === Nat.succ ts)
        (story' === (Cell.cell ts value vf) % story)

    let last_valueo t value =
      fresh (ts ts' story msg tail vf)
        (t   === loc_story  ts  story)
        (msg === Cell.cell ts' value vf)
        (story === msg % tail)

  end

module MemStory =
  struct
    type tt = (Lang.Loc.tt, LocStory.tt) Storage.tt

    type tl = (Lang.Loc.tl, LocStory.tl) Storage.tl
      and inner = (Lang.Loc.tl, LocStory.tl) Storage.inner

    type ti = (Lang.Loc.tt, LocStory.tt, Lang.Loc.tl, LocStory.tl) Storage.ti

    let allocate locs = Storage.allocate (LocStory.allocate ()) locs

    let init lv = Storage.from_assoc @@ List.map (fun (l, v) -> (l, LocStory.init v)) lv

    let reify = Storage.reify (Lang.Loc.reify) (LocStory.reify)

    let pprint ff story =
      let pp ff (loc, story) = Format.fprintf ff "%s: %a" (Lang.Loc.show loc) LocStory.pprint story in
      Format.fprintf ff "@[<v>Memory :@;%a@;@]" (Storage.pprint pp) story

    let shapeo = Storage.shapeo

    let snapshoto t xs =
      let make_constro (l, v) =
        (l, fun story -> LocStory.last_valueo story v)
      in
      Storage.constro t @@ List.map make_constro xs

    let last_tso t loc ts =
      fresh (story)
        (Storage.geto t loc story)
        (LocStory.last_tso story ts)

    let next_tso t loc ts =
      fresh (story)
        (Storage.geto t loc story)
        (LocStory.next_tso story ts)

    let loado t loc last_ts ts value vf =
      fresh (story)
        (Storage.geto t loc story)
        (LocStory.loado story last_ts ts value vf)

    let storeo t t' loc value vf  =
      fresh (story story')
        (Storage.geto t loc story)
        (Storage.seto t t' loc story')
        (LocStory.storeo story story' value vf)

    let last_valueo t loc value =
      fresh (story)
        (Storage.geto t loc story)
        (LocStory.last_valueo story value)

  end
