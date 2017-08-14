open MiniKanren
open MiniKanrenStd
open Utils

module Storage =
  struct
    type ('at, 'bt) tt = (('at * 'bt), ('at, 'bt) tt) llist

    type ('al, 'bl) tl = ('al, 'bl) inner MiniKanren.logic
      and ('al, 'bl) inner = (('al, 'bl) MiniKanren.Pair.logic, ('al, 'bl) tl) llist

    type ('at, 'bt, 'al, 'bl) ti = (('at, 'bt) tt, ('al, 'bl) tl) MiniKanren.injected

    type ('at, 'al) key = ('at, 'al) MiniKanren.injected
    type ('bt, 'bl) value = ('bt, 'bl) MiniKanren.injected

    let allocate vars default = inj_listi @@ List.map (fun var -> Pair.pair var default) vars

    let from_assoc assoc = inj_listi @@ List.map (fun (k, v) -> Pair.pair k v) assoc

    let inj inj_key inj_value s =
      MiniKanren.List.inj (fun (k, v) -> Pair.pair (inj_key k) (inj_value v)) s

    let pprint pp_kv = pprint_llist (pprint_logic pp_kv)

    let rec geto vars var value =
      fresh (hd tl)
        (vars === hd % tl)
        (conde [
          (hd === Pair.pair var value);
          (hd =/= Pair.pair var value) &&& (geto tl var value);
        ])

    let rec seto vars vars' var value =
      fresh (hd tl tl' k v)
        (vars === hd % tl)
        (hd === Pair.pair k v)
        (conde [
          (k === var) &&& (vars' === (Pair.pair var value) % tl);
          (k =/= var) &&& (vars' === hd % tl') &&& (seto tl tl' var value);
        ])

    let rec updateo upo t t' var =
      fresh (k v v' tl tl')
        (t  === (Pair.pair k v ) % tl )
        (t  === (Pair.pair k v') % tl')
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

module ThreadLocalStorage =
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

    type tt = ('at, 'at tt) Tree.t

    type tl = inner MiniKanren.logic
      and inner = ('al, 'al tl) Tree.t

    type ('at, 'al) ti = ('at tt, 'al tl) MiniKanren.injected

    include Fmap2(Tree)

    let nil () = inj @@ distrib @@ Tree.Nil

    let node ?(left=nil()) ?(right=nil()) x = inj @@ distrib @@ Tree.Node (x, left, right)

    let leaf x = node x

    let inj' = inj

    let rec inj inj_content tree = inj' @@ distrib (Tree.fmap (inj_content) (inj) tree)

    (* let rec to_logic tree = Value (Tree.fmap (ThreadState.to_logic) (to_logic) tree) *)

    let reify' = reify

    let rec reify h = reify' ThreadState.reify reify h

    (* let create ?rel ?acq vars curr =
      Tree.Node (ThreadState.create ?rel ?acq vars curr, Tree.Nil, Tree.Nil) *)

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
        | Var (i, _) ->
          lst := (Var (i, [])) :: !lst
      done;
      List.rev !lst

    let pprint pprint_content ff thrd_tree =
      let cnt = ref 1 in
      let pp ff thrd =
        Format.fprintf ff "@[<v>Thread %d:@;<1 4>%a@;@]" !cnt pprint_content thrd;
        cnt := !cnt + 1
      in
      List.iter (pp ff) @@ threads_list thrd_tree

    let rec geto tree path thrd = Lang.(ThreadID.(
      fresh (thrd' l r path')
        (tree === node thrd' l r)
        (conde [
          (path === pathn ()) &&& (thrd === thrd');
          (conde [
            (path === pathl path') &&& (geto l path' thrd);
            (path === pathr path') &&& (geto r path' thrd);
          ])
        ])
      ))

    let rec seto tree tree' path thrd_new = Lang.(ThreadID.(
      fresh (thrd thrd' path' l l' r r')
        (tree  === node thrd  l  r )
        (tree' === node thrd' l' r')
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

    let rec spawno spawn_contento tree tree' path = Lang.(ThreadID.(
      fresh (thrd l l' r r' path')
        (tree  === node thrd  l  r )
        (tree' === node thrd  l' r')
        (conde [
          fresh (a b)
            (path === pathn ())
            (l  === nil)
            (r  === nil)
            (l' === leaf a)
            (r' === leaf b)
            (spawn_contento thrd a b);

          (conde [
            (path === pathl path') &&& (spawno l l' path') &&& (r === r');
            (path === pathr path') &&& (spawno r r' path') &&& (l === l');
          ])
        ])
      ))

    let rec joino join_contento tree tree' path = Lang.(ThreadID.(
      fresh (thrd thrd' l l' r r' path')
        (tree  === node thrd  l  r )
        (tree' === node thrd' l' r')
        (conde [
          fresh (a b)
            (path  === pathn ())
            (l  === leaf a)
            (r  === leaf b)
            (l' === nil)
            (r' === nil)
            (join_contento thrd thrd' a b);

          (thrd === thrd') &&&
          (conde [
            (path === pathl path') &&& (r === r') &&& (joino l l' path');
            (path === pathr path') &&& (l === l') &&& (joino r r' path');
          ]);
        ])
      ))
  end

module Timestamp :
  sig
    type tt = Nat.ground

    type tl = inner MiniKanren.logic
      and inner = tl lnat

    type ti = (tt, tl) MiniKanren.injected

    let ts = inj_nat

    let inj = Nat.inj

    let show = GT.show(Nat.logic)
  end

module RegisterStorage =
  struct
    type tt = (Lang.Var.tt, Lang.Value.tt) Storage.tt

    type tl = (Lang.Var.tl, Lang.Value.tl) Storage.tl

    type ti = (Lang.Var.tt, Lang.Value.tt, Lang.Var.tl, Lang.Value.tl) Storage.ti

    let allocate = Storage.allocate (inj_nat 0)

    let from_assoc = Storage.from_assoc

    let inj = Storage.inj Lang.Register.inj Lang.Value.inj

    let reify h = ManualReifiers.(List.reify (pair (string) (Nat.reify)) h)

    let pprint = Storage.pprint (fun ff (k, v) -> Format.fprintf ff "%a=%a" pprint_string var pprint_nat value)

    let reado  = Storage.geto
    let writeo = Storage.seto

    let reseto = Storage.mapo (fun k v k' v' ->
      (k === k') &&& (v' === inj_nat 0)
    )
  end

module ViewFront =
  struct
    type tt = (Lang.Loc.tt, Timestamp.tt) Storage.tt

    type tl = (Lang.Loc.tl, Timestamp.tl) Storage.tl

    type ti = (Lang.Loc.tt, Timestamp.tt, Lang.Loc.tl, Timestamp.tl) Storage.ti

    let bottom = Storage.inj []

    let allocate = Storage.allocate (Nat.inj 0)

    let from_assoc = Storage.from_assoc

    let inj = Storage.inj Lang.Loc.inj Timestamp.inj

    let pprint = Storage.pprint (fun ff (k, v) -> Format.fprintf ff "%a@%a" pprint_string var pprint_nat value)

    (* let to_logic = List.to_logic (fun (loc, ts) -> Value (Value loc, Nat.to_logic ts)) *)

    let reify h = ManualReifiers.(List.reify (pair (string) (Nat.reify)) h)

    let tso = Storage.geto

    let updateo t t' loc ts =
      let r ts_old ts_new = conde [
          (ts >  ts_old) &&& (ts_new === ts);
          (ts <= ts_old) &&& (ts_new === ts_old);
      ] in
      Storage.updateo r t t' loc

    let mergeo t1 t2 t' =
      let r l1 ts1 l2 ts2 l' ts' = Nat.(conde [
        (ts1 >  ts2) &&& (ts' === ts1);
        (ts1 <= ts2) &&& (ts' === ts2);
      ]) in
      conde [
        (t1 === bottom ()) &&& (t2 === bottom ()) &&& (t' === bottom);
        (t1 === bottom ()) &&& (t2 =/= bottom ()) &&& (t' === t2);
        (t1 =/= bottom ()) &&& (t2 === bottom ()) &&& (t' === t1);
        (t1 =/= bottom ()) &&& (t2 =/= bottom ()) &&& (Storage.map2o r t1 t2 t');
      ]
  end

module ThreadFront =
  struct
    module T = struct
      type ('a, 'b, 'c, 'd, 'e) t = {
        regs : 'a;
        curr : 'b;
        rel  : 'c;
        acq  : 'd;
        prm  : 'e;
      }

      let fmap fa fb fc fd fe {regs = a; curr = b; rel = c; acq = d; prm = e} =
        {regs = fa a; curr = fb b; rel = fc c; acq = fd d; prm = fe e}
    end

    type tt = (RegisterStorage.tt, ViewFront.tt, ViewFront.tt, ViewFront.tt, PromiseSet.tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (RegisterStorage.tl, ViewFront.tl, ViewFront.tl, ViewFront.tl, PromiseSet.tl) T.t

    type ti = (tt, tl) MiniKanren.injected

    include Fmap5(T)

    let thrd_state regs curr rel acq prm =
      inj @@ distrib @@ {T.regs = regs; T.curr = curr; T.rel = rel; T.acq = acq; T.prm = prm }

    let inj {T.regs = regs; T.curr = curr; T.rel = rel; T.acq = acq; T.prm = prm } =
      thrd_state (RegisterStorage.inj regs) (ViewFront.inj curr) (ViewFront.inj rel) (ViewFront.inj acq) (PromiseSet.inj prm)

    (* let to_logic { T.regs = regs; T.curr = curr; T.rel = rel; T.acq = acq; T.prm = prm } =
      Value {
        T.regs = RegisterStorage.to_logic regs;
        T.curr = ViewFront.to_logic curr;
        T.rel  = ViewFront.to_logic rel;
        T.acq  = ViewFront.to_logic acq;
        T.prm  = PromiseSet.to_logic prm;
      } *)

    let reify h = reify RegisterStorage.reify ViewFront.reify ViewFront.reify ViewFront.reify PromiseSet.reify h

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

    let preallocate vars atomics = {
      T.regs = RegisterStorage.allocate vars;
      T.curr = ViewFront.allocate atomics;
      T.rel  = ViewFront.allocate atomics;
      T.acq  = ViewFront.allocate atomics;
      T.prm  = Nil;
    }

    let pprint =
      let pp ff {T.regs = regs; T.curr = curr; T.rel = rel; T.acq = acq; T.prm = prm } =
        Format.fprintf ff "@[<v>reg: %a @; cur: %a @; acq: %a @; rel: %a @; prm: %a @]"
          RegisterStorage.printer regs
          ViewFront.printer curr
          ViewFront.printer acq
          ViewFront.printer rel
          PromiseSet.printer prm
      in
      pprint_logic pp

    let reado thrd var value =
      fresh (regs curr rel acq prm)
        (thrd === thrd_state regs curr rel acq prm)
        (RegisterStorage.reado regs var value)

    let writeo thrd thrd' var value =
      fresh (regs regs' curr rel acq prm)
        (thrd  === thrd_state regs  curr rel acq prm)
        (thrd' === thrd_state regs' curr rel acq prm)
        (RegisterStorage.writeo regs regs' var value)

    let tso thrd loc ts =
      fresh (regs curr rel acq prm)
        (thrd === thrd_state regs curr rel acq prm)
        (ViewFront.tso curr loc ts)

    let updateo thrd thrd' loc ts =
      fresh (regs curr curr' rel acq acq' prm)
        (thrd  === thrd_state regs curr  rel  acq  prm)
        (thrd' === thrd_state regs curr' rel  acq' prm)
        (ViewFront.updateo curr curr' loc ts)
        (* (VarList.seto rel  rel'  loc ts) *)
        (ViewFront.updateo acq  acq'  loc ts)

    let front_relo thrd loc rel =
      fresh (regs curr acq prm)
        (thrd === thrd_state regs curr rel acq prm)

    let update_acqo thrd thrd' vf =
      fresh (regs curr rel acq acq' prm)
        (thrd  === thrd_state regs curr rel acq  prm)
        (thrd' === thrd_state regs curr rel acq' prm)
        (ViewFront.mergeo vf acq acq')

    let fence_acqo thrd thrd' =
      fresh (regs curr rel acq prm)
        (thrd  === thrd_state regs curr rel acq prm)
        (thrd' === thrd_state regs acq  rel acq prm)

    let fence_relo ?loc thrd thrd' =
      fresh (regs curr rel acq prm)
        (thrd  === thrd_state regs curr rel  acq prm)
        (thrd' === thrd_state regs curr curr acq prm)

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
      fresh (regs regs' curr rel acq)
        (thrd   === thrd_state regs  curr rel acq (nil ()))
        (child1 === thrd_state regs' curr rel acq (nil ()))
        (child1 === child2)
        (RegisterStorage.reseto regs regs')

    let joino thrd thrd' child1 child2 =
      fresh (regs       regs1 regs2
             curr curr' curr1 curr2
             rel  rel' rel1 rel2
             acq  acq' acq1 acq2
             prm prm1 prm2)
        (thrd   === thrd_state regs  curr  rel  acq  (nil ()))
        (thrd'  === thrd_state regs  curr' rel' acq' (nil ()))
        (child1 === thrd_state regs1 curr1 rel1 acq1 (nil ()))
        (child2 === thrd_state regs2 curr2 rel2 acq2 (nil ()))
        (ViewFront.mergeo curr1 curr2 curr')
        (ViewFront.mergeo rel1  rel2  rel' )
        (ViewFront.mergeo acq1  acq2  acq' )
        (* (List.appendo prm1 prm2 prm) *)

  end

module LocStory =
  struct
    module Cell = struct
      type tt = (Timestamp.tt * Lang.Value.tt * ViewFront.tt)

      type tl = inner MiniKanren.logic
        and inner = (Timestamp.tl * Lang.Value.tl * ViewFront.tl)

      type ti = (tt, tl) MiniKanren.injected

      let inj (ts, value, vf) =
        inj_triple (Timestamp.inj ts) (Value.inj value) (ViewFront.inj vf)

      (* let to_logic (ts, value, vf) = Value (Nat.to_logic ts, Nat.to_logic value, ViewFront.to_logic vf) *)

      let reify = ManualReifiers.triple Nat.reify Nat.reify ViewFront.reify

      let pprint var =
        let pp ff (ts, value, vf) =
          Format.fprintf ff "@[<h>{%a@%a=%a, %a}@]"
            pprint_string var
            pprint_nat ts
            pprint_nat value
            ViewFront.printer vf
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

    type tl = inner T.t MiniKanren.logic
      and inner = (Timestamp.tl, Cell.tl List.logic)

    type ti = (tt, tl) MiniKanren.injected

    include Fmap2(T)

    let loc_story tsnext story = inj @@ distrib @@ {T.tsnext = tsnext; T.story = story}

    let preallocate atomics =
      let vf = ViewFront.allocate atomics in
      create (Timestamp.ts 1) [(Timestamp.ts 0, Value.value 0, vf)]

    let inj {T.tsnext = tsnext; T.story = story} =
      loc_story (Timestamp.inj tsnext) (List.inj (Cell.inj) story)

    (* let to_logic {T.tsnext = tsnext; T.story = story} =
      Value {T.tsnext = Nat.to_logic tsnext; T.story = List.to_logic Cell.to_logic story} *)

    let reify h = reify Nat.reify (List.reify Cell.reify) h

    let create tsnext story = {
      T.tsnext = Nat.of_int tsnext;
      T.story  = List.of_list (fun (ts, v, vf) -> (Nat.of_int ts, Nat.of_int v, vf)) story;
    }

    let pprint var =
      let pp ff {T.story = story} =
        pprint_llist (Cell.printer var) ff story
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
        (msg === inj_triple ts' value vf)
        (Nat.leo ts ts' b)

    let loado t last_ts ts value vf =
      fresh (story tsnext visible msg)
        (t === loc_story tsnext story)
        (MiniKanrenStd.List.filtero (visibleo last_ts) story visible)
        (MiniKanrenStd.List.membero visible msg)
        (msg === inj_triple ts value vf)

    let storeo t t' value vf =
      fresh (ts ts' story story')
        (t  === loc_story ts  story )
        (t' === loc_story ts' story')
        (ts' === Nat.succ ts)
        (story' === (inj_triple ts value vf) % story)

    let last_valueo t value =
      fresh (ts ts' story msg tail vf)
        (t   === loc_story  ts  story)
        (msg === inj_triple ts' value vf)
        (story === msg % tail)

  end

module MemStory =
  struct
    type tt = (Lang.Loc.tt, LocStory.tt) Storage.tt

    type tl = (Lang.Loc.tl, LocStory.tl) Storage.tl

    type ti = (Lang.Loc.tt, LocStory.tt, Lang.Loc.tl, LocStory.tl) Storage.ti

    let preallocate atomics = Storage.allocate (LocStory.preallocate atomics)

    let inj = Storage.inj (Lang.Loc.inj) (LocStory.inj)

    (* let to_logic = List.to_logic (fun (var, story) -> Value (Value var, LocStory.to_logic story)) *)

    let reify h = ManualReifiers.(List.reify (pair (string) (LocStory.reify)) h)

    (* let create = List.of_list (fun x -> x) *)

    let pprint ff story =
      let pp ff (var, story) = LocStory.printer var ff story in
      Format.fprintf ff "@[<v>Memory :@;<1 4>%a@;@]" (pprint_llist (pprint_logic pp)) story

    let last_tso t loc ts =
      fresh (story)
        (Storage.geto t loc story)
        (LocStory.last_tso story ts)

    let next_tso t loc ts =
      fresh (story)
        (Storage.geto t loc story)
        (LocStory.next_tso story ts)

    let reado t loc last_ts ts value vf =
      fresh (story)
        (Storage.geto t loc story)
        (LocStory.reado story last_ts ts value vf)

    let writeo t t' loc value vf  =
      fresh (story story')
        (Storage.geto t loc story)
        (Storage.seto t t' loc story')
        (LocStory.writeo story story' value vf)

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
