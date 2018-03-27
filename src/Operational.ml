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

open Lang
open Utils

module MemoryModel =
  struct
    module type T =
      sig
        include Utils.Logic

        val alloc : thrdn:int -> string list -> ti
        val init  : thrdn:int -> (string * int) list -> ti

        val checko : ti -> Loc.ti -> Value.ti -> MiniKanren.goal

        val stepo : ThreadID.ti -> Label.ti -> ti -> ti -> MiniKanren.goal
      end

    module type MinT =
      sig
        include Utils.Logic

        val init  : thrdn:int -> (string * int) list -> ti

        val checko : ti -> Loc.ti -> Value.ti -> MiniKanren.goal

        val stepo : ThreadID.ti -> Label.ti -> ti -> ti -> MiniKanren.goal
      end

    module Make(M : MinT) =
      struct
        include M

        let alloc ~thrdn locs = init ~thrdn @@ List.map (fun l -> (l, 0)) locs
      end

  end

module State(Memory : MemoryModel.T) =
  struct
    module T =
      struct
        @type ('thrds, 'mem, 'opterr) t =
          { thrds  : 'thrds
          ; mem    : 'mem
          ; opterr : 'opterr
          }
        with gmap

        let fmap fa fb fc x = GT.gmap(t) fa fb fc x
      end

    type tt = (ThreadManager.tt, Memory.tt, Error.tt Std.Option.ground) T.t

    type tl = inner MiniKanren.logic
      and inner = (ThreadManager.tl, Memory.tl, Error.tl Std.Option.logic) T.t

    type ti = (tt, tl) MiniKanren.injected
    type ri = (tt, tl) MiniKanren.reified

    module F = Fmap3(T)

    let reify = F.reify
      ThreadManager.reify
      Memory.reify
      (Std.Option.reify Error.reify)

    let pprint =
      let pp ff = let open T in fun {thrds; mem; opterr} ->
        Format.fprintf ff "@[<v>";
        pprint_logic (fun ff -> function
          | None      -> ()
          | Some err  ->
            Format.fprintf ff "Error: %a@;" Error.pprint err
        ) ff opterr;
        (* Format.fprintf ff "@[<v>Registers:@;<1 2>%a@]@;@[<v>Memory:@;<1 2>%a@]" *)
        Format.fprintf ff "%a@\nMemory:@;<1 2>%a"
          ThreadManager.pprint thrds
          Memory.pprint mem;
        Format.fprintf ff "@]@;"
      in
      pprint_logic pp

    let state thrds mem opterr = T.(inj @@ F.distrib {thrds; mem; opterr})

    let init thrds mem = state thrds mem (Std.none ())

    let memo s mem =
      fresh (thrds opterr)
        (s === state thrds mem opterr)

    let thrdmgro s thrds =
      fresh (mem opterr)
        (s === state thrds mem opterr)

    let erroro ?(sg=fun _ -> success) ?(fg=failure) s =
      fresh (thrds mem opterr err)
        (s === state thrds mem opterr)
        (conde
          [ (opterr === Std.some err) &&& (sg err)
          ; (opterr === Std.none () ) &&& fg
          ]
        )

    let safeo s =
      fresh (thrds mem)
        (s === state thrds mem (Std.none ()))

    let dataraceo s =
      erroro s ~sg:(fun err -> fresh (mo loc) (err === Error.datarace mo loc))

    let rec sato p s = conde
    [ (p === Prop.true_  ()) &&& success
    ; (p === Prop.false_ ()) &&& failure

    ; fresh (p')
        (p === Prop.neg p')
      ?~(sato p' s)

    ; fresh (p1 p2)
        (p === Prop.conj p1 p2)
        ((sato p1 s) &&& (sato p2 s))

    ; fresh (p1 p2)
        (p === Prop.disj p1 p2)
        ((sato p1 s) ||| (sato p2 s))

    ; (erroro s
        ~sg:(fun err -> conde
          [ fresh (mo loc)
              (err === Error.datarace mo loc)
              (p === Prop.datarace ())
          ; fresh (e)
              (err === Error.assertion e)
              (p === Prop.assertion ())
          ]
        )
        ~fg:(conde
          [ fresh (tid r v tm mem opterr thrd rs)
              (p === Prop.reg_eq tid r v)
              (s === state tm mem opterr)
              (ThreadManager.geto tm tid thrd)
              (Thread.regso thrd rs)
              (Regs.reado rs r v)

          ; fresh (tid l v thrds mem opterr)
              (p === Prop.loc_eq l v)
              (s === state thrds mem opterr)
              (Memory.checko mem l v)
          ]
        )
      )
    ]

  end

module Interpreter(Memory : MemoryModel.T) =
  struct
    module State = State(Memory)

    open State

    let stepo s s' =
      fresh (tm tm' mem mem' opterr tid label)
        (s  === state tm  mem  (Std.none ()))
        (s' === state tm' mem' opterr)
        (ThreadManager.stepo tid label tm tm')
        (Memory.stepo tid label mem mem')
        (Label.erroro label
          ~sg:(fun err -> opterr === Std.some err)
          ~fg:(opterr === Std.none ())
        )

    let reachableo =
      let reachableo_norec reachableo_rec s s'' = conde
        [ (s === s'')
        ; fresh (s')
            (stepo s s')
            (reachableo_rec s' s'')
        ]
      in
      Tabling.(tabledrec two) reachableo_norec

    let evalo s s' =
      fresh (tm' mem' opterr')
        (s' === state tm' mem' opterr')
        (reachableo s s')
        (ThreadManager.terminatedo tm')

    let eval ?(prop = Prop.true_ ()) s =
      run q
        (fun s' -> (evalo s s') &&& (sato prop s'))
        (fun ss -> ss)

    let invarianto po s = ?~(
      fresh (s')
          (reachableo s s')
        ?~(po s')
    )

  end

(* ************************************************************************** *)
(* ********************** SequentialConsistent ****************************** *)
(* ************************************************************************** *)

module ValueStorage =
  struct
    type tt = (Lang.Loc.tt, Lang.Value.tt) Storage.tt

    type tl = (Lang.Loc.tl, Lang.Value.tl) Storage.tl
      and inner = (Lang.Loc.tl, Lang.Value.tl) Storage.inner

    type ti = (Lang.Loc.tt, Lang.Value.tt, Lang.Loc.tl, Lang.Value.tl) Storage.ti
    type ri = (tt, tl) MiniKanren.reified

    let allocate = Storage.allocate (Lang.Value.integer 0)

    let from_assoc = Storage.from_assoc

    let shapeo = Storage.shapeo

    let reify = Storage.reify (Lang.Loc.reify) (Lang.Value.reify)

    let pprint =
      Storage.pprint (fun ff (k, v) -> Format.fprintf ff "%s=%s" (Lang.Loc.show k) (Lang.Value.show v))

    let reado  = Storage.geto
    let writeo = Storage.seto
  end

module SeqCst = MemoryModel.Make(
  struct
    type tt = ValueStorage.tt

    type tl = ValueStorage.tl
      and inner = ValueStorage.inner

    type ti = ValueStorage.ti
    type ri = (tt, tl) MiniKanren.reified

    let reify = ValueStorage.reify

    let init ~thrdn mem =
      let mem = List.map (fun (l, v) -> (Loc.loc l, Value.integer v)) mem in
      ValueStorage.from_assoc mem

    let pprint = ValueStorage.pprint

    let checko t l v =
      (ValueStorage.reado t l v)

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

    let stepo tid label t t' = conde [
      (conde
        [ (label === Label.empty ())
        ; fresh (e) (label === Label.error (Error.assertion e))
        ]
      ) &&& (t === t');

      fresh (mo loc v)
        (* (mo === !!MemOrder.SC) *)
        (label === Label.load mo loc v)
        (load_sco t t' tid loc v);

      fresh (mo loc v)
        (* (mo === !!MemOrder.SC) *)
        (label === Label.store mo loc v)
        (store_sco t t' tid loc v);

      fresh (mo1 mo2 loc e d v)
        (* (mo1 === !!MemOrder.SC) *)
        (* (mo2 === !!MemOrder.SC) *)
        (label === Label.cas mo1 mo2 loc e d v)
        (cas_sco t t' tid loc e d v);
    ]
  end)

(* ************************************************************************** *)
(* ************************* ReleaseAcquire ********************************* *)
(* ************************************************************************** *)

open MiniKanren.Std

module Timestamp =
  struct
    type tt = Nat.ground

    type tl = inner MiniKanren.logic
      and inner = tl nat

    type ti = (tt, tl) MiniKanren.injected
    type ri = (tt, tl) MiniKanren.reified

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
    type ri = (Lang.Loc.tt, Timestamp.tt, Lang.Loc.tl, Timestamp.tl) Storage.ri

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
    type ri = (tt, tl) MiniKanren.reified

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
        Format.fprintf ff "@[<v>cur: %a@;acq: %a@;rel: %a@]"
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
        type ri = (tt, tl) MiniKanren.reified

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
      Format.fprintf ff "@[<v>Memory Story :@;%a@;@]" (Storage.pprint pp) story

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

module RelAcq = MemoryModel.Make(
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

    module TLS = Lang.ThreadLocalStorage(ThreadFront)

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
    type ri = (tt, tl) MiniKanren.reified

    include Fmap4(T)

    let state thrds story na sc = inj @@ distrib @@ T.({thrds; story; na; sc;})

    let reify = reify TLS.reify MemStory.reify ViewFront.reify ViewFront.reify

    let init ~thrdn mem =
      let mem   = List.map (fun (l, v) -> (Loc.loc l, Value.integer v)) mem in
      let locs  = List.map fst mem in
      let thrd  = ThreadFront.allocate locs in
      let thrds = TLS.init thrdn thrd in
      let story = MemStory.init mem in
      let na    = ViewFront.allocate locs in
      let sc    = ViewFront.allocate locs in
      state thrds story na sc

    let pprint =
      let pp ff = let open T in fun {thrds; story; na; sc} ->
        Format.fprintf ff "@[<v>%a@;%a@;@[<v>NA-front :@;<1 4>%a@;@]@;@[<v>SC-front :@;<1 4>%a@;@]@]"
          TLS.pprint thrds
          MemStory.pprint story
          ViewFront.pprint na
          ViewFront.pprint sc
      in
      pprint_logic pp

    let checko t l v =
      fresh (thrds story na sc)
        (t === state thrds story na sc)
        (MemStory.last_valueo story l v)

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
        (TLS.geto thrds tid thrd)
        (ThreadFront.tso thrd loc ts)
        (MemStory.last_tso story loc ts)
        (na_awareo na loc ts)
        (MemStory.loado story loc ts ts value vf)

    let store_nao t t' tid loc value =
      fresh (thrds thrds' story story' na na' sc thrd thrd' ts ts' rel vf)
        (t  === state thrds  story  na  sc)
        (t' === state thrds' story' na' sc)
        (TLS.geto thrds        tid thrd)
        (TLS.seto thrds thrds' tid thrd')
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
        (TLS.geto thrds tid thrd)
        (ThreadFront.tso thrd loc ts)
        (conde [
          (na_stucko na loc ts);
          (not_last_tso story loc ts);
        ])

    let dataraceo t t' tid loc =
      fresh (thrds story na sc thrd ts)
        (t === t')
        (t === state thrds story na sc)
        (TLS.geto thrds tid thrd)
        (ThreadFront.tso thrd loc ts)
        (na_stucko na loc ts)

    let load_rlxo t t' tid loc value ts =
      fresh (thrds thrds' story story' na sc thrd thrd' thrd'' last_ts vf)
        (t  === state thrds  story na sc)
        (t' === state thrds' story na sc)
        (TLS.geto thrds        tid thrd  )
        (TLS.seto thrds thrds' tid thrd'')
        (ThreadFront.tso thrd loc last_ts)
        (na_awareo na loc last_ts)
        (MemStory.loado story loc last_ts ts value vf)
        (ThreadFront.update_acqo thrd thrd' vf)
        (ThreadFront.updateo thrd' thrd'' loc ts)

    let store_rlxo t t' tid loc value ts =
      fresh (thrds thrds' story story' na sc thrd thrd' last_ts rel)
        (t  === state thrds  story  na sc)
        (t' === state thrds' story' na sc)
        (TLS.geto thrds        tid thrd)
        (TLS.seto thrds thrds' tid thrd')
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
        (TLS.geto thrds        tid thrd )
        (TLS.seto thrds thrds' tid thrd')
        (ThreadFront.fence_acqo  thrd thrd')

    let fence_relo ?loc t t' tid =
      fresh (thrds thrds' thrd thrd' story na sc)
        (t  === state  thrds  story na sc)
        (t' === state  thrds' story na sc)
        (TLS.geto thrds       tid thrd )
        (TLS.seto thrds thrds' tid thrd')
        (ThreadFront.fence_relo  thrd thrd' ?loc)

    let load_acqo t t'' tid loc value ts =
      fresh (t')
        (load_rlxo t t' tid loc value ts)
        (fence_acqo t' t'' tid)

    let store_relo t t'' tid loc value ts =
      fresh (t')
        (fence_relo t t' tid ~loc)
        (store_rlxo t' t'' tid loc value ts)

    let cas_rao t t'' tid loc expected desired value =
      fresh (t' ts ts')
        (load_acqo t t' tid loc value ts)
        (conde
          [ (Lang.Value.nqo value expected) &&& (t' === t'')
          ; fresh (thrds story na sc  ts')
              (t' === state thrds story na sc)
              (Lang.Value.eqo value expected)
              (MemStory.last_tso story loc ts)
              (store_relo t' t'' tid loc desired ts')
          ]
        )

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

    let stepo tid label t t' = conde [
      (conde
        [ (label === Label.empty ())
        ; fresh (e) (label === Label.error (Error.assertion e))
        ]
      ) &&& (t === t');

      fresh (mo loc v)
        (label === Label.load mo loc v)
        (conde [
          (mo === !!MemOrder.SC ) &&& (load_acqo t t' tid loc v);
          (mo === !!MemOrder.ACQ) &&& (load_acqo t t' tid loc v);
          (mo === !!MemOrder.RLX) &&& (load_rlxo t t' tid loc v);
          (mo === !!MemOrder.NA ) &&& (load_nao  t t' tid loc v);
        ]);

      fresh (mo loc v)
        (label === Label.store mo loc v)
        (conde [
          (mo === !!MemOrder.SC ) &&& (store_relo t t' tid loc v);
          (mo === !!MemOrder.REL) &&& (store_relo t t' tid loc v);
          (mo === !!MemOrder.RLX) &&& (store_rlxo t t' tid loc v);
          (mo === !!MemOrder.NA ) &&& (store_nao  t t' tid loc v);
        ]);

      fresh (mo1 mo2 loc exp des v)
        (label === Label.cas mo1 mo2 loc exp des v)
        (* TODO: different mo combinations *)
        (cas_rao t t' tid loc exp des v);

      fresh (loc mo v)
        (label === Label.error (Error.datarace mo loc))
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
  end)
