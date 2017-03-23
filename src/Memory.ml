open MiniKanren
open Lang

module Registers =
  struct
    type tt = (string, MiniKanren.Nat.ground) VarList.tt
    type tl = (string MiniKanren.logic, MiniKanren.Nat.logic) VarList.tl
    type ti = (string, MiniKanren.Nat.ground, string MiniKanren.logic, MiniKanren.Nat.logic) VarList.ti

    let inj = List.inj (fun (var, value) -> inj_pair (!!var) (inj_nat @@ Nat.to_int value))

    let allocate vars = List.of_list @@ List.map (fun s -> (s, Nat.of_int 0)) vars
  end

module ViewFront =
  struct
    type tt = (string, MiniKanren.Nat.ground) VarList.tt
    type tl = (string MiniKanren.logic, MiniKanren.Nat.logic) VarList.tl
    type ti = (string, MiniKanren.Nat.ground, string MiniKanren.logic, MiniKanren.Nat.logic) VarList.ti

    let inj = List.inj (fun (var, value) -> inj_pair (!!var) (inj_nat @@ Nat.to_int value))

    let allocate atomics = List.of_list @@ List.map (fun s -> (s, Nat.of_int 0)) atomics

    let from_list lst = List.of_list @@ List.map (fun (s, v) -> (s, Nat.of_int v)) lst
  end

module ThreadState =
  struct
    module T = struct
      type ('a, 'b) t = {
        regs : 'a;
        curr : 'b;
      }

      let fmap fa fb {regs = a; curr = b} = {regs = fa a; curr = fb b}
    end

    type tt = (Registers.tt, ViewFront.tt) T.t

    type tl_inner = (Registers.tl, ViewFront.tl) T.t

    type tl = tl_inner MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    module Fmap = Fmap2(T)

    let thrd_state regs curr = inj @@ Fmap.distrib @@ {T.regs = regs; T.curr = curr}

    let inj {T.regs = regs; T.curr = curr} = thrd_state (Registers.inj regs) (ViewFront.inj curr)

    let convert = List.map (fun (var, value) -> (var, Nat.of_int value))

    let create vars vf = {
      T.regs = List.of_list (convert vars);
      T.curr = List.of_list (convert vf);
    }

    let preallocate vars atomics = {
      T.regs = Registers.allocate vars;
      T.curr = ViewFront.allocate atomics;
    }

    let get_varo thrd var value =
      fresh (regs curr)
        (thrd === thrd_state regs curr)
        (VarList.geto regs var value)

    let set_varo thrd thrd' var value =
      fresh (regs regs' curr)
        (thrd  === thrd_state regs  curr)
        (thrd' === thrd_state regs' curr)
        (VarList.seto regs regs' var value)

    let get_tso thrd var ts =
      fresh (regs curr)
        (thrd === thrd_state regs curr)
        (VarList.geto curr var ts)

    let set_tso thrd thrd' var ts =
      fresh (regs curr curr')
        (thrd  === thrd_state regs curr)
        (thrd' === thrd_state regs curr')
        (VarList.seto curr curr' var ts)

    let updateo thrd thrd' vf =
      fresh (regs curr curr')
        (thrd  === thrd_state regs curr)
        (thrd' === thrd_state regs curr')
        (VarList.joino VarList.join_tso vf curr curr')

    let spawno thrd spwn1 spwn2 =
      (spwn1 === thrd) &&& (spwn2 === thrd)

    let joino thrd1 thrd2 vf =
      fresh (regs1 regs2 curr1 curr2)
        (thrd1 === thrd_state regs1 curr1)
        (thrd2 === thrd_state regs2 curr2)
        (VarList.joino VarList.join_tso curr1 curr2 vf)

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

    let nil        = inj @@ Fmap.distrib @@ Nil
    let node a l r = inj @@ Fmap.distrib @@ Node (a, l, r)
    let leaf a     = inj @@ Fmap.distrib @@ Node (a, nil, nil)

    let inj' = inj

    let rec inj tree = inj' @@ Fmap.distrib (Tree.fmap (ThreadState.inj) (inj) tree)

    let rec geto tree path thrd =
      fresh (thrd' l r path')
        (tree === node thrd' l r)
        (conde [
          (path === Lang.pathn) &&& (thrd === thrd');
          (conde [
            (path === Lang.pathl path') &&& (geto l path' thrd);
            (path === Lang.pathr path') &&& (geto r path' thrd);
          ])
        ])

    let rec seto tree tree' path thrd_new =
      fresh (thrd thrd' path' l l' r r')
        (tree  === node thrd  l  r )
        (tree' === node thrd' l' r')
        (conde [
          (path === Lang.pathn) &&& (thrd' === thrd_new) &&&
          (l === l') &&& (r === r');

          (thrd' === thrd) &&&
          (conde [
            (path === Lang.pathl path') &&& (r === r') &&& (seto l l' path' thrd_new);
            (path === Lang.pathr path') &&& (l === l') &&& (seto r r' path' thrd_new);
          ])
        ])

    let rec spawno tree tree' path =
      fresh (thrd l l' r r' path')
        (tree  === node thrd  l  r )
        (tree' === node thrd  l' r')
        (conde [
          fresh (a b)
            (path === Lang.pathn)
            (l  === nil)
            (r  === nil)
            (l' === leaf a)
            (r' === leaf b)
            (ThreadState.spawno thrd a b);

          (conde [
            (path === Lang.pathl path') &&& (spawno l l' path') &&& (r === r');
            (path === Lang.pathr path') &&& (spawno r r' path') &&& (l === l');
          ])
        ])

    let rec joino tree tree' path =
      fresh (thrd thrd' l l' r r' path')
        (tree  === node thrd  l  r )
        (tree' === node thrd' l' r')
        (conde [
          fresh (a b vf regs curr)
            (path  === Lang.pathn)
            (l  === leaf a)
            (r  === leaf b)
            (l' === nil)
            (r' === nil)
            (ThreadState.joino a b vf)
            (thrd  === ThreadState.thrd_state regs curr)
            (thrd' === ThreadState.thrd_state regs vf);

          (thrd === thrd') &&&
          (conde [
            (path === Lang.pathl path') &&& (r === r') &&& (joino l l' path');
            (path === Lang.pathr path') &&& (l === l') &&& (joino r r' path');
          ]);
        ])
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
      T.story  = List.of_list @@ List.map (fun (ts, v, vf) -> (Nat.of_int ts, Nat.of_int v, vf)) story;
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

    let read_acqo t last_ts ts value vf =
      fresh (story tsnext visible msg)
        (t === loc_story tsnext story)
        (MiniKanren.List.filtero (visibleo last_ts) story visible)
        (MiniKanren.List.membero visible msg)
        (msg === inj_triple ts value vf)

    let write_relo t t' value vf =
      fresh (ts ts' story story')
        (t  === loc_story ts  story )
        (t' === loc_story ts' story')
        (ts' === Nat.s ts)
        (story' === (inj_triple ts value vf) % story)

  end

(*
module MemStory =
  struct
    type t   = (loc * LocStory.t) list
    type lt' = ((loc logic * LocStory.lt) logic, lt' logic) llist
    type lt  = lt' logic

    let empty = []

    let preallocate atomics = List.map (fun a -> (a, LocStory.empty)) atomics

    let from_assoc assoc = assoc

    let (!) = (!!)

    let inj t  = MiniKanren.List.inj (fun (l, story) -> !(!l, LocStory.inj story)) @@ MiniKanren.List.of_list t

    let prj lt = MiniKanren.List.to_list @@ MiniKanren.List.prj (Utils.prj_pair (!?) LocStory.prj) lt

    let show t = List.fold_left (fun a (l, story) -> l ^ ": " ^ (LocStory.show story) ^ "\n") "" t

    let eq t t' =
      let
        check_exists (l, story) = List.exists (fun (l', story') -> (l = l') && (LocStory.eq story story')) t'
      in
        List.for_all check_exists t

    let next_tstmpo t l ts =
      fresh (story)
        (Utils.assoc_defaulto l t (LocStory.inj LocStory.empty) story)
        (LocStory.next_tstmpo story ts)

    let read_acqo t l ts ts' v vf =
      fresh (story)
        (Utils.assoco l t story)
        (LocStory.read_acqo story ts ts' v vf)

    let update_k v vf l opt_story story' =
      fresh (story)
        (conde [
          (opt_story === !(Some story));
          (opt_story === !None) &&& (story === LocStory.inj LocStory.empty);
        ])
        (LocStory.write_relo v vf story story')

    let write_relo l v vf t t' = Utils.update_assoco_k l (update_k v vf) t t'

    let read_acq t l ts = run qrs (fun q  r  s  -> read_acqo (inj t) (!l) (inj_nat ts) q r s)
                                  (fun qs rs ss -> Utils.zip3 (Stream.map prj_nat qs) (Stream.map prj_nat rs) (Stream.map ViewFront.prj ss))

    let write_rel l v vf t = run q (fun q  -> write_relo (!l) (inj_nat v) (ViewFront.inj vf) (inj t) q)
                                   (fun qs -> prj @@ Utils.excl_answ qs)

  end

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

module MemState =
  struct
    type t = {
      thrds : ThreadTree.t;
      story : MemStory.t;
      scmem : SCMemory.t
    }

    type lt' = {
      lthrds : ThreadTree.lt;
      lstory : MemStory.lt;
      lscmem : SCMemory.lt
    }

    type lt = lt' MiniKanren.logic

    let empty = { thrds = ThreadTree.empty; story = MemStory.empty; scmem = SCMemory.empty }

    let preallocate vars atomics = { thrds = ThreadTree.preallocate vars atomics;
                                     story = MemStory.preallocate atomics;
                                     scmem = SCMemory.preallocate atomics }

    let inj t = !! { lthrds = ThreadTree.inj t.thrds; lstory = MemStory.inj t.story; lscmem = SCMemory.inj t.scmem; }

    let prj lt =
      let lt' = !?lt in
      { thrds = ThreadTree.prj lt'.lthrds;
        story = MemStory.prj lt'.lstory;
        scmem = SCMemory.prj lt'.lscmem; }

    let sep = "-------------------------------------------------------------"

    let show t = Printf.sprintf "Threads:\n%s \n%s \nSCMem:\n%s \n%s \nMemory:\n%s \n%s" sep (ThreadTree.show t.thrds) sep (SCMemory.show t.scmem) sep (MemStory.show t.story)

    let eq t t' = (ThreadTree.eq t.thrds t'.thrds) && (MemStory.eq t.story t'.story)

    let (!) = (!!)

    let splito t thrd_tree story scmem =
      (t === !{ lthrds = thrd_tree; lstory = story; lscmem = scmem; })

    let get_thrdo path t thrd =
      fresh (thrd_tree h scmem)
        (splito t thrd_tree h scmem)
        (ThreadTree.get_thrdo path thrd_tree thrd)

    let update_thrdo path thrd t t' =
      fresh (thrd_tree thrd_tree' h scmem)
        (splito t thrd_tree h scmem)
        (ThreadTree.update_thrdo path thrd thrd_tree thrd_tree')
        (splito t' thrd_tree' h scmem)

    let assign_localo path x n t t' =
      fresh (thrd thrd')
        (get_thrdo path t thrd)
        (ThreadState.assign_localo x n thrd thrd')
        (update_thrdo path thrd' t t')

    let read_acqo path l v t t' =
      fresh (thrd_tree thrd_tree' story scmem thrd thrd' ts ts' vf)
        (splito t thrd_tree story scmem)
        (ThreadTree.get_thrdo path thrd_tree thrd)
        (ThreadState.get_tstmpo thrd l ts)
        (MemStory.read_acqo story l ts ts' v vf)
        (ThreadState.join_viewfronto vf thrd thrd')
        (ThreadTree.update_thrdo path thrd' thrd_tree thrd_tree')
        (splito t' thrd_tree' story scmem)

    let write_relo path l v t t' =
      fresh (thrd_tree thrd_tree' story story' scmem thrd thrd' ts vf)
        (splito t thrd_tree story scmem)
        (MemStory.next_tstmpo story l ts)
        (ThreadTree.get_thrdo path thrd_tree thrd)
        (ThreadState.update_tstmpo l ts thrd thrd' vf)
        (MemStory.write_relo l v vf story story')
        (ThreadTree.update_thrdo path thrd' thrd_tree thrd_tree')
        (splito t' thrd_tree' story' scmem)

    let read_sco path l v t t' =
      fresh (thrd_tree story scmem)
        (splito t thrd_tree story scmem)
        (SCMemory.geto l scmem v)
        (t' === t)

    let write_sco path l v t t' =
      fresh (thrd_tree story scmem scmem')
        (splito t thrd_tree story scmem)
        (SCMemory.seto l v scmem scmem')
        (splito t' thrd_tree story scmem')

    let spawn_thrdo path t t' =
      fresh (thrd_tree thrd_tree' h scmem)
        (splito t thrd_tree h scmem)
        (ThreadTree.spawn_thrdo path thrd_tree thrd_tree')
        (splito t' thrd_tree' h scmem)

    let join_thrdo path t t' =
      fresh (thrd_tree thrd_tree' h scmem)
        (splito t thrd_tree h scmem)
        (ThreadTree.join_thrdo path thrd_tree thrd_tree')
        (splito t' thrd_tree' h scmem)

    let get_thrd path t = run q (fun q  -> get_thrdo (Path.inj path) (inj t) q)
                                (fun qs -> ThreadState.prj @@ Utils.excl_answ qs)

    let assign_local path x n t = run q (fun q  -> assign_localo (Path.inj path) !x (inj_nat n) (inj t) q)
                                        (fun qs -> prj @@ Utils.excl_answ qs)

    let read_acq path l t = run qr (fun q  r  -> read_acqo (Path.inj path) (!l) q (inj t) r)
                                   (fun qs rs -> Stream.zip (Stream.map prj_nat qs) (Stream.map prj rs))

    let write_rel path l v t = run q (fun q  -> write_relo (Path.inj path) (!l) (inj_nat v) (inj t) q)
                                     (fun qs -> prj @@ Utils.excl_answ qs)
  end *)
