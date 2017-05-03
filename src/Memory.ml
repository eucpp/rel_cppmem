open MiniKanren
open Lang

module Registers =
  struct
    type tt = (string, MiniKanren.Nat.ground) VarList.tt
    type tl = (string MiniKanren.logic, MiniKanren.Nat.logic) VarList.tl
    type ti = (string, MiniKanren.Nat.ground, string MiniKanren.logic, MiniKanren.Nat.logic) VarList.ti

    let inj = List.inj (fun (var, value) -> inj_pair (!!var) (Nat.inj value))

    let allocate vars = List.of_list (fun s -> (s, Nat.of_int 0)) vars

    let reset_varo p p' = Nat.(
      fresh (var value)
        (p  === inj_pair var value)
        (p' === inj_pair var (inj_nat 0))
      )

    let reseto = VarList.mapo reset_varo
  end

module ViewFront =
  struct
    type tt = (string, MiniKanren.Nat.ground) VarList.tt
    type tl = (string MiniKanren.logic, MiniKanren.Nat.logic) VarList.tl
    type ti = (string, MiniKanren.Nat.ground, string MiniKanren.logic, MiniKanren.Nat.logic) VarList.ti

    let inj = List.inj (fun (var, value) -> inj_pair (!!var) (Nat.inj value))

    let allocate atomics = List.of_list (fun s -> (s, Nat.of_int 0)) atomics

    let from_list lst = List.of_list (fun (s, v) -> (s, Nat.of_int v)) lst

    let mergeo t1 t2 t = VarList.map2o VarList.join_tso t1 t2 t
  end

module ThreadState =
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

    type tt = (Registers.tt, ViewFront.tt, ViewFront.tt, ViewFront.tt) T.t

    type tl_inner = (Registers.tl, ViewFront.tl, ViewFront.tl, ViewFront.tl) T.t

    type tl = tl_inner MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    module Fmap = Fmap4(T)

    let thrd_state regs curr rel acq =
      inj @@ Fmap.distrib @@ {T.regs = regs; T.curr = curr; T.rel = rel; T.acq = acq}

    let inj {T.regs = regs; T.curr = curr; T.rel = rel; T.acq = acq} =
      thrd_state (Registers.inj regs) (ViewFront.inj curr) (ViewFront.inj rel) (ViewFront.inj acq)

    let convert = (fun (var, value) -> (var, Nat.of_int value))

    let create ?(rel) ?(acq) vars curr =
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
    }

    let preallocate vars atomics = {
      T.regs = Registers.allocate vars;
      T.curr = ViewFront.allocate atomics;
      T.rel  = ViewFront.allocate atomics;
      T.acq  = ViewFront.allocate atomics;
    }

    let get_varo thrd var value =
      fresh (regs curr rel acq)
        (thrd === thrd_state regs curr rel acq)
        (VarList.geto regs var value)

    let set_varo thrd thrd' var value =
      fresh (regs regs' curr rel acq)
        (thrd  === thrd_state regs  curr rel acq)
        (thrd' === thrd_state regs' curr rel acq)
        (VarList.seto regs regs' var value)

    let last_tso thrd loc ts =
      fresh (regs curr rel acq)
        (thrd === thrd_state regs curr rel acq)
        (VarList.geto curr loc ts)

    let updateo thrd thrd' loc ts =
      fresh (regs curr curr' rel rel' acq acq')
        (thrd  === thrd_state regs curr  rel  acq )
        (thrd' === thrd_state regs curr' rel  acq')
        (VarList.seto curr curr' loc ts)
        (* (VarList.seto rel  rel'  loc ts) *)
        (VarList.seto acq  acq'  loc ts)

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

    let fence_loc_relo thrd thrd' loc =
      fresh (regs curr rel acq)
        (thrd  === thrd_state regs curr rel  acq)
        (thrd' === thrd_state regs curr curr acq)

    let fence_relo thrd thrd' =
      fresh (regs curr rel acq)
        (thrd  === thrd_state regs curr rel  acq)
        (thrd' === thrd_state regs curr curr acq)

    let spawno thrd child1 child2 =
      fresh (regs regs' curr rel acq)
        (thrd   === thrd_state regs  curr rel acq)
        (child1 === thrd_state regs' curr rel acq)
        (child1 === child2)
        (Registers.reseto regs regs')

    let joino thrd thrd' child1 child2 =
      fresh (regs       regs1 regs2
             curr curr' curr1 curr2
             rel  rel' rel1 rel2
             acq  acq' acq1 acq2)
        (thrd   === thrd_state regs  curr  rel  acq )
        (thrd'  === thrd_state regs  curr' rel' acq')
        (child1 === thrd_state regs1 curr1 rel1 acq1)
        (child2 === thrd_state regs2 curr2 rel2 acq2)
        (ViewFront.mergeo curr1 curr2 curr')
        (ViewFront.mergeo rel1  rel2  rel' )
        (ViewFront.mergeo acq1  acq2  acq' )

  end

module Threads =
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

    type tt = (ThreadState.tt, tt) Tree.t
    type tl = (ThreadState.tl, tl) Tree.t MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    module Fmap = Fmap2(Tree)

    let nil        = inj @@ Fmap.distrib @@ Tree.Nil
    let node a l r = inj @@ Fmap.distrib @@ Tree.Node (a, l, r)
    let leaf a     = inj @@ Fmap.distrib @@ Tree.Node (a, nil, nil)

    let inj' = inj

    let rec inj tree = inj' @@ Fmap.distrib (Tree.fmap (ThreadState.inj) (inj) tree)

    let create ?rel ?acq vars curr =
      Tree.Node (ThreadState.create ?rel ?acq vars curr, Tree.Nil, Tree.Nil)

    let rec geto tree path thrd = Path.(
      fresh (thrd' l r path')
        (tree === node thrd' l r)
        (conde [
          (path === pathn ()) &&& (thrd === thrd');
          (conde [
            (path === pathl path') &&& (geto l path' thrd);
            (path === pathr path') &&& (geto r path' thrd);
          ])
        ])
      )

    let rec seto tree tree' path thrd_new = Path.(
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
      )

    let rec spawno tree tree' path = Path.(
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
            (ThreadState.spawno thrd a b);

          (conde [
            (path === pathl path') &&& (spawno l l' path') &&& (r === r');
            (path === pathr path') &&& (spawno r r' path') &&& (l === l');
          ])
        ])
      )

    let rec joino tree tree' path = Path.(
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
            (ThreadState.joino thrd thrd' a b);

          (thrd === thrd') &&&
          (conde [
            (path === pathl path') &&& (r === r') &&& (joino l l' path');
            (path === pathr path') &&& (l === l') &&& (joino r r' path');
          ]);
        ])
      )
  end

module LocStory =
  struct
    module Cell = struct
      type tt = (Nat.ground * Nat.ground * ViewFront.tt)
      type tl = (Nat.logic  * Nat.logic  * ViewFront.tl) logic
      type ti = (tt, tl) injected

      let inj (ts, value, vf) =
        inj_triple (inj_nat @@ Nat.to_int ts) (inj_nat @@ Nat.to_int value) (ViewFront.inj vf)
    end

    module T = struct
      type ('a, 'b) t = {
        tsnext : 'a;
        story  : 'b;
      }

      let fmap fa fb {tsnext = a; story = b} = {tsnext = fa a; story = fb b}
    end

    type tt = (Nat.ground, Cell.tt List.ground) T.t
    type tl_inner = (Nat.logic, Cell.tl List.logic) T.t
    type tl = tl_inner MiniKanren.logic
    type ti = (tt, tl) MiniKanren.injected

    module Fmap = Fmap2(T)

    let loc_story tsnext story = inj @@ Fmap.distrib @@ {T.tsnext = tsnext; T.story = story}

    let inj {T.tsnext = tsnext; T.story = story} = loc_story (inj_nat @@ Nat.to_int tsnext) (List.inj (Cell.inj) story)

    let create tsnext story = {
      T.tsnext = Nat.of_int tsnext;
      T.story  = List.of_list (fun (ts, v, vf) -> (Nat.of_int ts, Nat.of_int v, vf)) story;
    }

    let preallocate atomics =
      let vf = ViewFront.allocate atomics in
      create 1 [(0, 0, vf)]

    let next_tso t ts =
      fresh (story)
        (t === loc_story ts story)

    let visibleo ts msg b =
      fresh (ts' value vf)
        (msg === inj_triple ts' value vf)
        (Nat.leo ts ts' b)

    let reado t last_ts ts value vf =
      fresh (story tsnext visible msg)
        (t === loc_story tsnext story)
        (MiniKanren.List.filtero (visibleo last_ts) story visible)
        (MiniKanren.List.membero visible msg)
        (msg === inj_triple ts value vf)

    let writeo t t' value vf =
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
    type tt = (Lang.Loc.tt, LocStory.tt) VarList.tt
    type tl = (Lang.Loc.tl, LocStory.tl) VarList.tl
    type ti = (Lang.Loc.tt, LocStory.tt, Lang.Loc.tl, LocStory.tl) VarList.ti

    let inj = List.inj (fun (loc, story) -> inj_pair (!!loc) (LocStory.inj story))

    let create = List.of_list (fun x -> x)

    let preallocate atomics = List.of_list (fun loc -> (loc, LocStory.preallocate atomics)) atomics

    let next_tso t loc ts =
      fresh (story)
        (VarList.geto t loc story)
        (LocStory.next_tso story ts)

    let reado t loc last_ts ts value vf =
      fresh (story)
        (VarList.geto t loc story)
        (LocStory.reado story last_ts ts value vf)

    let writeo t t' loc value vf  =
      fresh (story story')
        (VarList.geto t loc story)
        (VarList.seto t t' loc story')
        (LocStory.writeo story story' value vf)

    let last_valueo t loc value =
      fresh (story story)
        (VarList.geto t loc story)
        (LocStory.last_valueo story value)

  end

(*
module SCMemory =
  struct
    type t   = (string * int) list
    type lt' = ((string logic * Nat.logic) logic, lt' logic) llist
    type lt  = lt' logic

    let empty = []

    let preallocate atomics = List.map (fun a -> (a, 0)) atomics

    let (!) = MiniKanren.inj

    let inj = Utils.inj_assoc (!)  (inj_nat)
    let prj = Utils.prj_assoc (!?) (prj_nat)

    let show = Utils.show_assoc (fun x -> x) (string_of_int)

    let eq = Utils.eq_assoc (=) (=)

    let geto = Utils.assoco

    let seto = Utils.update_assoco

    let get var regs = run q (fun q  -> geto !var (inj regs) q)
                             (fun qs -> prj_nat @@ Utils.excl_answ qs)

    let set var v regs = run q (fun q  -> seto !var (inj_nat v) (inj regs) q)
                               (fun qs -> (prj @@ Utils.excl_answ qs))
  end

*)


module MemState =
  struct
    module T = struct
      type ('a, 'b) t = {
        thrds : 'a;
        story : 'b;
      }

      let fmap fa fb {thrds = a; story = b} = {thrds = fa a; story = fb b}
    end

    type tt = (Threads.tt, MemStory.tt) T.t
    type tl_inner = (Threads.tl, MemStory.tl) T.t
    type tl = tl_inner logic
    type ti = (tt, tl) MiniKanren.injected

    module Fmap = Fmap2(T)

    let mem_state thrds story = inj @@ Fmap.distrib @@ {T.thrds = thrds; T.story = story}

    let inj {T.thrds = thrds; T.story = story} = mem_state (Threads.inj thrds) (MemStory.inj story)

    let create thrds story = {
      T.thrds = thrds;
      T.story  = story;
    }

    let preallocate vars atomics =
      let thrd  = ThreadState.preallocate vars atomics in
      let thrds = Threads.Tree.Node (thrd, Threads.Tree.Nil, Threads.Tree.Nil) in
      let story = MemStory.preallocate atomics in
      create thrds story

    let get_thrdo t path thrd =
      fresh (tree story)
        (t === mem_state tree story)
        (Threads.geto tree path thrd)

    let set_thrdo t t' path thrd =
      fresh (tree tree' story)
        (t  === mem_state tree  story)
        (t' === mem_state tree' story)
        (Threads.seto tree tree' path thrd)

    let get_localo t path var value =
      fresh (thrd)
        (get_thrdo t path thrd)
        (ThreadState.get_varo thrd var value)

    let set_localo t t' path var value =
      fresh (thrd thrd')
        (get_thrdo t path thrd)
        (set_thrdo t t' path thrd')
        (ThreadState.set_varo thrd thrd' var value)

    let read_rlxo t t' path loc value =
      fresh (tree tree' story story' thrd thrd' thrd'' last_ts ts vf)
        (t  === mem_state tree  story)
        (t' === mem_state tree' story)
        (Threads.geto tree path thrd)
        (Threads.seto tree tree' path thrd'')
        (ThreadState.last_tso thrd loc last_ts)
        (MemStory.reado story loc last_ts ts value vf)
        (ThreadState.updateo thrd thrd' loc ts)
        (ThreadState.update_acqo thrd thrd'' vf)

    let write_rlxo t t' path loc value =
      fresh (tree tree' story story' thrd thrd' ts rel)
        (t  === mem_state tree  story)
        (t' === mem_state tree' story')
        (Threads.geto tree path thrd)
        (Threads.seto tree tree' path thrd')
        (MemStory.next_tso story loc ts)
        (ThreadState.updateo thrd thrd' loc ts)
        (ThreadState.front_relo thrd' loc rel)
        (MemStory.writeo story story' loc value rel)

    let fence_acqo t t' path =
      fresh (tree tree' story thrd thrd')
        (t  === mem_state  tree  story)
        (t' === mem_state  tree' story)
        (Threads.geto tree       path thrd )
        (Threads.seto tree tree' path thrd')
        (ThreadState.fence_acqo  thrd thrd')

    let fence_relo t t' path =
      fresh (tree tree' story thrd thrd')
        (t  === mem_state  tree  story)
        (t' === mem_state  tree' story)
        (Threads.geto tree       path thrd )
        (Threads.seto tree tree' path thrd')
        (ThreadState.fence_relo  thrd thrd')

    let fence_loc_relo t t' path loc =
      fresh (tree tree' story thrd thrd')
        (t  === mem_state  tree  story)
        (t' === mem_state  tree' story)
        (Threads.geto tree       path thrd )
        (Threads.seto tree tree' path thrd')
        (ThreadState.fence_loc_relo thrd thrd' loc)

    let read_acqo t t'' path loc value =
      fresh (t')
        (read_rlxo t t' path loc value)
        (fence_acqo t' t'' path)

    let write_relo t t'' path loc value =
      fresh (t')
        (fence_loc_relo t t' path loc)
        (write_rlxo t' t'' path loc value)

    let last_valueo t loc value =
      fresh (tree tree story)
        (t  === mem_state tree story)
        (MemStory.last_valueo story loc value)

    let spawno t t' path =
      fresh (tree tree' story)
        (t  === mem_state tree  story)
        (t' === mem_state tree' story)
        (Threads.spawno tree tree' path)

    let joino t t' path =
      fresh (tree tree' story)
        (t  === mem_state tree  story)
        (t' === mem_state tree' story)
        (Threads.joino tree tree' path)
  end
