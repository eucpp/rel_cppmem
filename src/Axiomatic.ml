open MiniKanren
open MiniKanrenStd
open Utils
open Lang
open Lang.Term

module EventID =
  struct
    type tt = tt Std.nat
    type tl = inner MiniKanren.logic
      and inner = tl Std.nat
    type ti = MiniKanrenStd.Nat.groundi

    let cnt = ref Nat.zero

    let init () = Nat.zero

    let eid = nat

    let next () =
      cnt := Nat.succ !cnt;
      !cnt

    let reify = Nat.reify

    (* let rec show n =
      let rec to_ground : tl -> int = function
      | Value (S n) -> 1 + (to_ground n)
      | Value (O)   -> 0
      | Var (i, _)  -> invalid_arg "Free Var"
      in
      string_of_int @@ to_ground n *)

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

    let pprint ff v = Format.fprintf ff "%s" @@ show v
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

    let event eid label = inj @@ distrib @@ {T.eid = eid; T.label = label}

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

    let extendo (eid, eid') ord ord' =
      (ord' === (pair eid eid')%ord)

    let mergeo = List.appendo

    (* computes transitive closure *)
    let trcl ord =
      let r_norec r_rec eid eid'' = conde [
        (List.membero ord @@ pair eid eid'');

        fresh (eid')
          (List.membero ord @@ pair eid eid')
          (r_rec eid' eid'');
      ] in
      Tabling.(tabledrec two r_norec)
  end

module EventSet =
  struct
    type tt = Event.tt List.ground
    type tl = inner MiniKanren.logic
      and inner = (Event.tl, tl) MiniKanren.Std.list
    type ti = (tt, tl) MiniKanren.injected

    let empty = nil

    let reify = List.reify Event.reify

    let pprint = pprint_llist Event.pprint

    let mergeo = List.appendo
  end

(* module ThreadCounter =
  struct
    type tt = (Lang.ThreadID.tt, EventID.tt) Memory.Storage.tt
    type tl = inner MiniKanren.logic
      and inner (Lang.ThreadID.tl, EventID.tl) Memory.Storage.tl
    type ti = (tt, tl) MiniKanren.injected

    let init () =
      Storage.allocate (EventID.init ()) ThreadID.([pathl @@ pathn (); pathr @@ pathn ()])

    let reify = Storage.reify Lang.ThreadID.reify EventID.reify

    let pprint = Storage.pprint Lang.ThreadID.pprint EventID.pprint

    let eido = geto

    let updateo = seto
  end *)

module Graph =
  struct
    module T =
      struct
        @type ('es, 'ord) t =
          { events  : 'es
          ; po      : 'ord
          }
        with gmap

        let fmap f g x = GT.gmap(t) f g x
      end

    type tt = (EventSet.tt, Order.tt) T.t
    type tl = inner MiniKanren.logic
      and inner = (EventSet.tl, Order.tl) T.t
    type ti = (tt, tl) MiniKanren.injected

    include Fmap2(T)

    let graph events po =
      T.(inj @@ distrib @@ {events; po})

    let empty () =
      let events = EventSet.empty () in
      let po     = Order.empty () in
      graph events po

    let reify h = reify EventSet.reify Order.reify h

    let pprint =
      let pp ff { T.po = po } =
        Format.fprintf ff "@[<v>po: %a @;@]@."
          Order.pprint po
      in
      pprint_logic pp

    let extendo label g g' =
      let open Event in
      conde [
        (label === Label.empty ()) &&& (g === g');

        fresh (e eid tid es es' po po')
          (g  === graph es  po )
          (g' === graph es' po')
          (e  === event eid label)
          (eid === EventID.next ())
          (es' === e % es)
          (conde [
            fresh (eid' eid'' tl)
              (po  === (pair eid'' eid') % tl)
              (po' === (pair eid'  eid ) % po);

            (* fresh ()  *)
              (po === nil ())  &&&
              (po' === (pair (EventID.init ()) eid) % po)
              (* (conde [
                ();
                ();
              ])  *)
          ])
      ]

    let mergeo g1 g2 g =
      fresh (es es1 es2 po po1 po2)
        (g  === graph es  po )
        (g1 === graph es1 po1)
        (g2 === graph es2 po2)
        (EventSet.mergeo es1 es2 es)
        (Order.mergeo po1 po2 po)
  end

module SequentialConsistent =
  struct
    module Node = Semantics.MakeConfig(Lang.Term)(Graph)

    let lift_split splito term ctx rdx =
      fresh (term' rdx' ctx' rs graph)
        (term === Node.cfg term' graph)
        (rdx  === Node.cfg rdx'  graph)
        (ctx  === Rules.Context.context ctx' rs)
        (splito term' ctx' rdx')

    let lift_plug plugo ctx rdx term =
      fresh (term' rdx' ctx' rs graph)

        (term === Node.cfg term' graph)
        (rdx  === Node.cfg rdx'  graph)
        (ctx  === Rules.Context.context ctx' rs)
        (plugo ctx' rdx' term')
    let lift_rule rule ctx ctx' t t' =
      fresh (label prog prog' graph graph')
        (t  === Node.cfg prog  graph )
        (t' === Node.cfg prog' graph')
        (rule label ctx ctx' prog prog')
        (Graph.extendo label graph graph')

    let stepo = Semantics.Reduction.make_step
      (lift_split Lang.splito)
      (lift_plug Lang.plugo)
      (List.map lift_rule (Rules.Basic.all @ Rules.Atomic.all))

    let eval_thrdo =
      let irreducibleo t =
        fresh (prog graph)
          (t === Node.cfg prog graph)
          (Lang.Term.irreducibleo prog)
      in
      Semantics.Reduction.make_eval ~irreducibleo stepo

    let pre_execo t g =
      fresh (p1 p2 t1 t1' t2 t2' g1 g2)
        (t   === spw p1 p2)
        (t1  === Node.cfg p1 (Graph.empty ()))
        (t2  === Node.cfg p2 (Graph.empty ()))
        (t1' === Node.cfg (skip ()) g1)
        (t2' === Node.cfg (skip ()) g2)
        (eval_thrdo t1 t1')
        (eval_thrdo t2 t2')
        (Graph.mergeo g1 g2 g)

    let rf_wfo es rf =
      let open Event in
      let rec helpero rs ws rf = conde [
        (rs === nil ()) &&& (rf === nil ());

        fresh (w r rs' rf' reid weid tid1 tid2 mo1 mo2 loc v)
          (r  === event reid @@ Label.load  tid1 mo1 loc v)
          (w  === event weid @@ Label.store tid2 mo2 loc v)
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

    let totalo ord es =
      let open Event in
      fresh (e e' eid eid' label label')
        (e === event eid  label )
        (e === event eid' label')
        (e =/= e')
        (List.membero es e )
        (List.membero es e')
        ((ord eid eid') ||| (ord eid' eid))

    let acyclico ord es =
      let open Event in
      fresh (e eid label)
        (e === event eid label)
        (List.membero es e)
      ?~(ord eid eid)

    let sc_execo t a b =
      let open EventID in
      fresh (g es po rf sc h h')
        (let sc_trcl = Order.trcl sc in
          (* (g === Graph.graph es po) &&&
          (pre_execo t g) &&&
          (rf_wfo es rf) &&&
          (Order.mergeo po rf sc) &&& *)

          (Order.extendo (eid 1, eid 2) h  h') &&&
          (Order.extendo (eid 2, eid 1) h' sc) &&&

          (* (totalo sc_trcl es) &&& (acyclico sc_trcl es) &&& *)

          (sc_trcl a b)
        )

  end
