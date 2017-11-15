open MiniKanren
open MiniKanrenStd
open Utils

module Storage =
  struct
    type ('at, 'bt) tt = (('at * 'bt), ('at, 'bt) tt) MiniKanren.Std.list

    type ('al, 'bl) tl = ('al, 'bl) inner MiniKanren.logic
      and ('al, 'bl) inner = (('al, 'bl) MiniKanren.Std.Pair.logic, ('al, 'bl) tl) MiniKanren.Std.list

    type ('at, 'bt, 'al, 'bl) ti = (('at, 'bt) tt, ('al, 'bl) tl) MiniKanren.injected

    type ('at, 'al) key = ('at, 'al) MiniKanren.injected
    type ('bt, 'bl) value = ('bt, 'bl) MiniKanren.injected

    let empty = MiniKanren.Std.nil

    let allocate default vars = MiniKanren.Std.List.list @@ List.map (fun var -> pair var default) vars

    let from_assoc assoc = MiniKanren.Std.List.list @@ List.map (fun (k, v) -> pair k v) assoc

    let reify reify_key reify_value = List.reify (Pair.reify reify_key reify_value)

    let inj inj_key inj_value s =
      MiniKanren.Std.List.inj (fun (k, v) -> to_logic (inj_key k, inj_value v)) s

    let pprint pp_kv = pprint_llist (pprint_logic pp_kv)

    let rec geto vars var value =
      fresh (hd tl)
        (vars === hd % tl)
        (conde [
          (hd === pair var value);
          (hd =/= pair var value) &&& (geto tl var value);
        ])

    let rec seto vars vars' var value =
      fresh (hd tl tl' k v)
        (vars === hd % tl)
        (hd === pair k v)
        (conde [
          (k === var) &&& (vars' === (pair var value) % tl);
          (k =/= var) &&& (vars' === hd % tl') &&& (seto tl tl' var value);
        ])

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
  end

module RegisterStorage =
  struct
    type tt = (Lang.Register.tt, Lang.Value.tt) Storage.tt

    type tl = (Lang.Register.tl, Lang.Value.tl) Storage.tl
      and inner = (Lang.Register.tl, Lang.Value.tl) Storage.inner

    type ti = (Lang.Register.tt, Lang.Value.tt, Lang.Register.tl, Lang.Value.tl) Storage.ti

    let allocate = Storage.allocate (Lang.Value.integer 0)

    let from_assoc = Storage.from_assoc

    let reify = Storage.reify (Lang.Register.reify) (Lang.Value.reify)

    let inj = Storage.inj Lang.Register.inj Lang.Value.inj

    let pprint =
      Storage.pprint (fun ff (k, v) -> Format.fprintf ff "%s=%s" (Lang.Register.show k) (Lang.Value.show v))

    let reado  = Storage.geto
    let writeo = Storage.seto

    let reseto = Storage.mapo (fun k v k' v' ->
      (k === k') &&& (v' === Lang.Value.integer 0)
    )

    let spawno regs regs1 regs2 =
        (regs1 === regs2) &&&
        (reseto regs regs1)

    let joino regs regs' regs1 regs2 =
        (regs === regs')
  end

module ValueStorage =
  struct
    type tt = (Lang.Loc.tt, Lang.Value.tt) Storage.tt

    type tl = (Lang.Loc.tl, Lang.Value.tl) Storage.tl
      and inner = (Lang.Loc.tl, Lang.Value.tl) Storage.inner

    type ti = (Lang.Loc.tt, Lang.Value.tt, Lang.Loc.tl, Lang.Value.tl) Storage.ti

    let allocate = Storage.allocate (Lang.Value.integer 0)

    let from_assoc = Storage.from_assoc

    let reify = Storage.reify (Lang.Loc.reify) (Lang.Value.reify)

    let inj = Storage.inj Lang.Loc.inj Lang.Value.inj

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

    let inj = Nat.inj

    (* let show = GT.show(Nat.logic) *)
    let show n =
      pprint_nat Format.str_formatter n;
      Format.flush_str_formatter ()

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

    let bottom = MiniKanren.Std.nil

    let allocate = Storage.allocate (Timestamp.ts 0)

    let from_assoc = Storage.from_assoc

    let reify = Storage.reify (Lang.Loc.reify) (Timestamp.reify)

    let inj = Storage.inj Lang.Loc.inj Timestamp.inj

    let pprint =
      Storage.pprint (fun ff (k, v) -> Format.fprintf ff "%s@%s" (Lang.Loc.show k) (Timestamp.show v))

    (* let to_logic = List.to_logic (fun (loc, ts) -> Value (Value loc, Nat.to_logic ts)) *)

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
      type ('a, 'b, 'c, 'd) t = {
        regs : 'a;
        curr : 'b;
        rel  : 'c;
        acq  : 'd;
      }

      let fmap fa fb fc fd {regs = a; curr = b; rel = c; acq = d} =
        {regs = fa a; curr = fb b; rel = fc c; acq = fd d}
    end

    type tt = (RegisterStorage.tt, ViewFront.tt, ViewFront.tt, ViewFront.tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (RegisterStorage.tl, ViewFront.tl, ViewFront.tl, ViewFront.tl) T.t

    type ti = (tt, tl) MiniKanren.injected

    include Fmap4(T)

    let thrd_state regs curr rel acq =
      inj @@ distrib @@ {T.regs = regs; T.curr = curr; T.rel = rel; T.acq = acq; }

    let inj x =
      to_logic @@ T.fmap (RegisterStorage.inj) (ViewFront.inj) (ViewFront.inj) (ViewFront.inj) x

    (* let to_logic { T.regs = regs; T.curr = curr; T.rel = rel; T.acq = acq; T.prm = prm } =
      Value {
        T.regs = RegisterStorage.to_logic regs;
        T.curr = ViewFront.to_logic curr;
        T.rel  = ViewFront.to_logic rel;
        T.acq  = ViewFront.to_logic acq;
        T.prm  = PromiseSet.to_logic prm;
      } *)

    let reify = reify RegisterStorage.reify ViewFront.reify ViewFront.reify ViewFront.reify

    let convert = (fun (var, value) -> (var, Nat.of_int value))

    (* let create ?(rel) ?(acq) vars curr =
      let rel = match rel with
        | Some rel -> rel
        | None     -> curr
      in
      let acq = match acq with
        | Some acq -> acq
        | None     -> curr
      in {
      T.regs = List.of_list convert vars;
      T.curr = List.of_list convert curr;
      T.rel  = List.of_list convert rel;
      T.acq  = List.of_list convert acq;
      T.prm  = Nil;
    } *)

    let preallocate vars atomics = thrd_state
      (RegisterStorage.allocate vars)
      (ViewFront.allocate atomics)
      (ViewFront.allocate atomics)
      (ViewFront.allocate atomics)

    let pprint =
      let pp ff { T.regs = regs; T.curr = curr; T.rel = rel; T.acq = acq; } =
        Format.fprintf ff "@[<v>reg: %a @;cur: %a @;acq: %a @;rel: %a @]"
          RegisterStorage.pprint regs
          ViewFront.pprint curr
          ViewFront.pprint acq
          ViewFront.pprint rel
      in
      pprint_logic pp

    let reado thrd var value =
      fresh (regs curr rel acq)
        (thrd === thrd_state regs curr rel acq)
        (RegisterStorage.reado regs var value)

    let writeo thrd thrd' var value =
      fresh (regs regs' curr rel acq)
        (thrd  === thrd_state regs  curr rel acq)
        (thrd' === thrd_state regs' curr rel acq)
        (RegisterStorage.writeo regs regs' var value)

    let tso thrd loc ts =
      fresh (regs curr rel acq)
        (thrd === thrd_state regs curr rel acq)
        (ViewFront.tso curr loc ts)

    let updateo thrd thrd' loc ts =
      fresh (regs curr curr' rel acq acq')
        (thrd  === thrd_state regs curr  rel  acq )
        (thrd' === thrd_state regs curr' rel  acq')
        (ViewFront.updateo curr curr' loc ts)
        (* (VarList.seto rel  rel'  loc ts) *)
        (ViewFront.updateo acq  acq'  loc ts)

    let front_relo thrd loc rel =
      fresh (regs curr acq)
        (thrd === thrd_state regs curr rel acq)

    let update_acqo thrd thrd' vf =
      fresh (regs curr rel acq acq')
        (thrd  === thrd_state regs curr rel acq )
        (thrd' === thrd_state regs curr rel acq')
        (ViewFront.mergeo vf acq acq')

    let fence_acqo thrd thrd' =
      fresh (regs curr rel acq)
        (thrd  === thrd_state regs curr rel acq)
        (thrd' === thrd_state regs acq  rel acq)

    let fence_relo ?loc thrd thrd' =
      fresh (regs curr rel acq)
        (thrd  === thrd_state regs curr rel  acq)
        (thrd' === thrd_state regs curr curr acq)

    (* let promiseo thrd thrd' loc ts value vf =
      fresh (regs curr rel acq prm prm' p)
        (thrd  === thrd_state regs curr rel  acq prm )
        (thrd' === thrd_state regs curr curr acq prm')
        (p === Promise.promise loc ts value vf)
        (prm' === p % prm) *)

    (* let betweeno ts_lb ts_ub ts =
      fresh (sum n d d')
        (Rational.lto ts_lb ts_ub !!true)
        (ts  === n %% d')
        (sum === n %% d )
        (Rational.addo ts_lb ts_ub sum)
        (Nat.mulo d 2 d') *)

    (* let removeo prm prm' p =
      let pred p' b = conde [
        (p =/= p') &&& (b === !!true);
        (p === p') &&& (b === !!false);
      ] in
      List.filtero pred prm prm'

    let fulfillo thrd thrd'' = Nat.(
      fresh (thrd' regs curr rel acq prm prm' p loc ts last_ts value vf)
        (thrd  === thrd_state regs curr rel acq prm )
        (thrd' === thrd_state regs curr rel acq prm')
        (List.membero prm p)
        (p === Promise.promise loc ts value vf)
        (tso thrd loc last_ts)
        (last_ts < ts)
        (* (vf === rel) *)
        (removeo prm prm' p)
        (updateo thrd' thrd'' loc ts)
      )

    let laggingo thrd b =
      fresh (regs curr rel acq prm)
        (thrd === thrd_state regs curr rel acq prm)
        (conde [
          (prm =/= inj_listi []) &&& (b === !!true);
          (prm === inj_listi []) &&& (b === !!false);
        ])

    let certifyo thrd =
      fresh (regs curr rel acq prm)
        (thrd  === thrd_state regs curr rel acq prm)
        (prm   === nil ()) *)

    let spawno thrd child1 child2 =
      fresh (regs regs1 regs2 curr rel acq)
        (thrd   === thrd_state regs  curr rel acq)
        (child1 === thrd_state regs1 curr rel acq)
        (child2 === thrd_state regs2 curr rel acq)
        (RegisterStorage.spawno regs regs1 regs2)

    let joino thrd thrd' child1 child2 =
      fresh (regs regs' regs1 regs2
             curr curr' curr1 curr2
             rel  rel' rel1 rel2
             acq  acq' acq1 acq2
             prm prm1 prm2)
        (thrd   === thrd_state regs  curr  rel  acq )
        (thrd'  === thrd_state regs' curr' rel' acq')
        (child1 === thrd_state regs1 curr1 rel1 acq1)
        (child2 === thrd_state regs2 curr2 rel2 acq2)
        (RegisterStorage.joino regs regs' regs1 regs2)
        (ViewFront.mergeo curr1 curr2 curr')
        (ViewFront.mergeo rel1  rel2  rel' )
        (ViewFront.mergeo acq1  acq2  acq' )
        (* (List.appendo prm1 prm2 prm) *)

  end

module type ThreadLocalData =
  sig
    include Utils.Logic

    val spawno : ti -> ti -> ti -> MiniKanren.goal
    val joino  : ti -> ti -> ti -> ti -> MiniKanren.goal
  end

module ThreadLocalStorage(T : ThreadLocalData) =
  struct
    module Tree =
      struct
        type ('a, 't) t =
          | Nil
          | Node of 'a * 't * 't

        let fmap fa ft = function
          | Nil            -> Nil
          | Node (a, l, r) -> Node (fa a, ft l, ft r)
      end

    type tt = (T.tt, tt) Tree.t

    type tl = inner MiniKanren.logic
      and inner = (T.tl, tl) Tree.t

    type ti = (tt, tl) MiniKanren.injected

    include Fmap2(Tree)

    let nil () = inj @@ distrib @@ Tree.Nil

    let node ?(left=nil()) ?(right=nil()) x = inj @@ distrib @@ Tree.Node (x, left, right)

    let leaf x = node x

    let reify' = reify
    let rec reify h = reify' T.reify reify h

    let threads_list thrd_tree =
      let q = Queue.create () in
      let lst = ref [] in
      Queue.push thrd_tree q;
      while not @@ Queue.is_empty q do
        match Queue.pop q with
        | Value (Tree.Node (thrd, l, r)) ->
          lst := thrd :: !lst;
          Queue.push l q;
          Queue.push r q
        | Value Tree.Nil  -> ()
        | Var (i, _) -> ()
      done;
      List.rev !lst

    let pprint ff thrd_tree =
      let cnt = ref 1 in
      let pp ff thrd =
        Format.fprintf ff "@[<v>Thread %d:@;<1 4>%a@;@]" !cnt T.pprint thrd;
        cnt := !cnt + 1
      in
      List.iter (pp ff) @@ threads_list thrd_tree

    let rec geto tree path thrd = Lang.(ThreadID.(
      fresh (thrd' left right path')
        (tree === node thrd' ~left ~right)
        (conde [
          (path === pathn ()) &&& (thrd === thrd');
          (conde [
            (path === pathl path') &&& (geto left  path' thrd);
            (path === pathr path') &&& (geto right path' thrd);
          ])
        ])
      ))

    let rec seto tree tree' path thrd_new = Lang.(ThreadID.(
      fresh (thrd thrd' path' l l' r r')
        (tree  === node thrd  ~left:l  ~right:r )
        (tree' === node thrd' ~left:l' ~right:r')
        (conde [
          (path === pathn ()) &&& (thrd' === thrd_new) &&&
          (l === l') &&& (r === r');

          (thrd' === thrd) &&&
          (conde [
            (path === pathl path') &&& (r === r') &&& (seto l l' path' thrd_new);
            (path === pathr path') &&& (l === l') &&& (seto r r' path' thrd_new);
          ])
        ])
      ))

    (* let rec laggingo tree b =
      fresh (thrd l r b1 b2)
        (conde [
            (tree === leaf thrd) &&&
            (ThreadState.laggingo thrd b);

            (tree =/= leaf thrd) &&&
            (tree === node thrd l r) &&&
            (laggingo l b1) &&&
            (laggingo r b2) &&&
            (MiniKanrenStd.Bool.oro b1 b2 b);
        ]) *)

    let rec spawno tree tree' path = Lang.(ThreadID.(
      fresh (thrd l l' r r' path')
        (tree  === node thrd  ~left:l  ~right:r )
        (tree' === node thrd  ~left:l' ~right:r')
        (conde [
          fresh (a b)
            (path === pathn ())
            (l  === nil ())
            (r  === nil ())
            (l' === leaf a)
            (r' === leaf b)
            (T.spawno thrd a b);

          (conde [
            (path === pathl path') &&& (spawno l l' path') &&& (r === r');
            (path === pathr path') &&& (spawno r r' path') &&& (l === l');
          ])
        ])
      ))

    let rec joino tree tree' path = Lang.(ThreadID.(
      fresh (thrd thrd' l l' r r' path')
        (tree  === node thrd  ~left:l  ~right:r )
        (tree' === node thrd' ~left:l' ~right:r')
        (conde [
          fresh (a b)
            (path  === pathn ())
            (l  === leaf a)
            (r  === leaf b)
            (l' === nil ())
            (r' === nil ())
            (T.joino thrd thrd' a b);

          (thrd === thrd') &&&
          (conde [
            (path === pathl path') &&& (r === r') &&& (joino l l' path');
            (path === pathr path') &&& (l === l') &&& (joino r r' path');
          ]);
        ])
      ))
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

        let inj x =
          to_logic @@ T.fmap Timestamp.inj Lang.Value.inj ViewFront.inj x

        (* let to_logic (ts, value, vf) = Value (Nat.to_logic ts, Nat.to_logic value, ViewFront.to_logic vf) *)

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

    let preallocate atomics =
      let vf = ViewFront.bottom () in
      inj @@ distrib @@ {
        T.tsnext = Timestamp.ts 1;
        T.story = MiniKanren.Std.List.list [Cell.cell (Timestamp.ts 0) (Lang.Value.integer 0) vf];
      }

    let reify = reify Timestamp.reify (List.reify Cell.reify)

    let inj x =
      to_logic @@ T.fmap (Timestamp.inj) (List.inj (Cell.inj)) x

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
        (MiniKanrenStd.List.filtero (visibleo last_ts) story visible)
        (MiniKanrenStd.List.membero visible msg)
        (msg === Cell.cell ts value vf)

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

    let preallocate atomics = Storage.allocate (LocStory.preallocate atomics) atomics

    let reify = Storage.reify (Lang.Loc.reify) (LocStory.reify)

    let inj = Storage.inj (Lang.Loc.inj) (LocStory.inj)

    (* let to_logic = List.to_logic (fun (var, story) -> Value (Value var, LocStory.to_logic story)) *)

    (* let create = List.of_list (fun x -> x) *)

    let pprint ff story =
      let pp ff (loc, story) = Format.fprintf ff "%s: %a" (Lang.Loc.show loc) LocStory.pprint story in
      Format.fprintf ff "@[<v>Memory :@;%a@;@]" (Storage.pprint pp) story

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
      fresh (story story)
        (Storage.geto t loc story)
        (LocStory.last_valueo story value)

  end

  (* module Promise =
    struct
      module T = struct
        type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

        let fmap fa fb fc fd (a, b, c, d) = (fa a, fb b, fc c, fd d)
      end

      type tt = (Lang.Loc.tt, Timestamp.tt, Lang.Value.tt, ViewFront.tt) T.t
      type tl = (Lang.Loc.tl, Timestamp.tl, Lang.Value.tl, ViewFront.tl) T.t MiniKanren.logic
      type ti = (tt, tl) MiniKanren.injected

      include Fmap4(T)

      let promise loc ts value vf =
        inj @@ distrib @@ (loc, ts, value, vf)

      let inj (loc, ts, value, vf) =
        promise (!!loc) (Nat.inj ts) (Nat.inj value) (ViewFront.inj vf)

      let to_logic (loc, ts, value, vf) = Value (Lang.Loc.to_logic loc, Nat.to_logic ts, Nat.to_logic value, ViewFront.to_logic vf)

      let reify = reify ManualReifiers.string Nat.reify Nat.reify ViewFront.reify

      let printer =
        let pp ff (loc, ts, value, vf) =
          Format.fprintf ff "@[<h>{%a@%a=%a, %a}@]"
            pprint_string loc
            pprint_nat ts
            pprint_nat value
            ViewFront.printer vf
        in
        pprint_logic pp

    end

  module PromiseSet =
    struct
      type tt = Promise.tt List.ground
      type tl = Promise.tl List.logic
      type ti = (Promise.tt, Promise.tl) List.groundi

      let inj = List.inj (Promise.inj)

      let to_logic = List.to_logic (Promise.to_logic)

      let reify = List.reify (Promise.reify)

      let printer =
        pprint_llist Promise.printer

    end *)
