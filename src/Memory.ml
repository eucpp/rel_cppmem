open MiniKanren
open Lang

module Registers =
  struct
    type tt = (string, MiniKanren.Nat.ground) VarList.tt
    type tl = (string MiniKanren.logic, MiniKanren.Nat.logic) VarList.tl
    type ti = (string, MiniKanren.Nat.ground, string MiniKanren.logic, MiniKanren.Nat.logic) VarList.ti
  end

module ViewFront =
  struct
    type tt = (string, MiniKanren.Nat.ground) VarList.tt
    type tl = (string MiniKanren.logic, MiniKanren.Nat.logic) VarList.tl
    type ti = (string, MiniKanren.Nat.ground, string MiniKanren.logic, MiniKanren.Nat.logic) VarList.ti
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

    let inj {T.regs = regs; T.curr = curr} =
      let regs' = prj_ground () regs in
      let curr' = prj_ground () curr in
      thrd_state regs' curr

    let create vars atomics =
      let inj_string_list = List.map (fun s -> !!s) in
      let rs = VarList.allocate (inj_string_list vars) (inj_nat 0) in
      let vf = VarList.allocate (inj_string_list atomics) (inj_nat 0) in
      thrd_state rs vf

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

    let rec inj tree = inj @@ Fmap.distrib (Tree.fmap () (inj) tree)

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

    let rec seto tree tree' path thrd' =
      fresh (thrd path' l l' r r')
        (tree  === node thrd  l  r )
        (tree' === node thrd' l' r')
        (conde [
          (path === Lang.pathn) &&& (l === l') &&& (r === r');
          (conde [
            (path === Lang.pathl path') &&& (r === r') &&& (seto l l' path' thrd');
            (path === Lang.pathr path') &&& (l === l') &&& (seto r r' path' thrd');
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

(*
module ThreadTree =
  struct
    @type ('a, 't) at = Leaf of 'a | Node of 't * 't with gmap

    type t   = (ThreadState.t, t) at
    type lt' = (ThreadState.lt, lt' logic) at
    type lt  = lt' logic

    let empty = Leaf ThreadState.empty

    let preallocate vars atomics = Leaf (ThreadState.preallocate vars atomics)

    let rec inj t  = !! (gmap(at) (ThreadState.inj) (inj) t)
    let rec prj lt = gmap(at) (ThreadState.prj) (prj) (!?lt)

    let rec thrd_list' thrds = function
    | Leaf thrd          -> thrd::thrds
    | Node (left, right) ->
      let thrds' = thrd_list' thrds left in
        thrd_list' thrds' right

    let thrd_list thrd_tree = List.rev @@ thrd_list' [] thrd_tree

    let show thrd_tree =
      let thrds = thrd_list thrd_tree in
      let sep = "-------------------------------------------------------------" in
      let cnt = ref 0 in
      let show_thrd acc thrd =
        cnt := !cnt + 1;
        acc ^ "Thread #" ^ (string_of_int !cnt) ^ ":\n" ^ (ThreadState.show thrd) ^ sep ^ "\n"
      in
        List.fold_left show_thrd "" thrds

    let rec eq thrd_tree thrd_tree' = match (thrd_tree, thrd_tree') with
      | Leaf thrd, Leaf thrd'      -> ThreadState.eq thrd thrd'
      | Node (l, r), Node (l', r') -> (eq l l') && (eq r r')
      | _, _ -> false

    let (!) = (!!)

    let rec get_thrdo path thrd_tree thrd = conde [
      (path === !Path.N) &&& (thrd_tree === !(Leaf thrd));
      fresh (l r path')
        (thrd_tree === !(Node (l, r)))
        (conde [
          (path === !(Path.L path')) &&& (get_thrdo path' l thrd);
          (path === !(Path.R path')) &&& (get_thrdo path' r thrd);
        ])
    ]

    let rec update_thrdo path thrd thrd_tree thrd_tree' = conde [
      fresh (thrd')
        ((path === !Path.N) &&& (thrd_tree === !(Leaf thrd')) &&& (thrd_tree' === !(Leaf thrd)));
      fresh (l r l' r' path')
        (thrd_tree === !(Node (l, r)))
        (conde [
          (path === !(Path.L path')) &&& (thrd_tree' === !(Node (l', r))) &&& (update_thrdo path' thrd l l');
          (path === !(Path.R path')) &&& (thrd_tree' === !(Node (l, r'))) &&& (update_thrdo path' thrd r r');
        ]);
    ]

    let rec spawn_thrdo path thrd_tree thrd_tree' = conde [
      fresh (thrd thrd' thrd'')
        (path === !Path.N)
        (thrd_tree === !(Leaf thrd))
        (ThreadState.spawno thrd thrd' thrd'')
        (thrd_tree' === !(Node (!(Leaf thrd'), !(Leaf thrd''))));
      fresh (l r l' r' path')
        (thrd_tree === !(Node (l, r)))
        (conde [
           (path === !(Path.L path')) &&& (thrd_tree' === !(Node (l', r ))) &&& (spawn_thrdo path' l l');
           (path === !(Path.R path')) &&& (thrd_tree' === !(Node (l , r'))) &&& (spawn_thrdo path' r r');
        ]);
    ]

    let rec join_thrdo path thrd_tree thrd_tree' = conde [
      fresh (l r joined)
        (path === !Path.N)
        (thrd_tree === !(Node (!(Leaf l), !(Leaf r))))
        (ThreadState.joino l r joined)
        (thrd_tree' === !(Leaf joined));
      fresh (l r l' r' path')
        (thrd_tree === !(Node (l, r)))
        (conde [
          (path === !(Path.L path') &&& (thrd_tree' === !(Node (l', r ))) &&& (join_thrdo path' l l'));
          (path === !(Path.R path') &&& (thrd_tree' === !(Node (l , r'))) &&& (join_thrdo path' r r'));
        ])
    ]

    let get_thrd path thrd_tree = run q (fun q  -> get_thrdo (Path.inj path) (inj thrd_tree) q)
                                        (fun qs -> ThreadState.prj @@ Utils.excl_answ qs)

    let update_thrd path thrd thrd_tree = run q (fun q  -> update_thrdo (Path.inj path) (ThreadState.inj thrd) (inj thrd_tree) q)
                                                (fun qs -> prj @@ Utils.excl_answ qs)

    let spawn_thrd path thrd_tree = run q  (fun q  -> spawn_thrdo (Path.inj path) (inj thrd_tree) q)
                                           (fun qs -> prj @@ Utils.excl_answ qs)

    let join_thrd path thrd_tree = run q (fun q  -> join_thrdo (Path.inj path) (inj thrd_tree) q)
                                         (fun qs -> prj @@ Utils.excl_answ qs)
  end


module Cell =
  struct
    type t   = (tstmp * int * ViewFront.t)
    type lt' = (Nat.logic * Nat.logic * ViewFront.lt)

    type lt = lt' logic

    let inj (ts, v, vf) = !! (inj_nat ts, inj_nat v, ViewFront.inj vf)
    let prj lt = !?lt |> fun (lts, lv, lvf) -> (prj_nat lts, prj_nat lv, ViewFront.prj lvf)

    let show (ts, v, vf) = "(" ^ (string_of_tstmp ts) ^ ", " ^ (string_of_int v) ^ ", " ^ ViewFront.show vf ^ ")"

    let eq (ts, v, vf) (ts', v', vf') = (ts = ts') && (v = v') && (ViewFront.eq vf vf')
  end

module LocStory =
  struct
    type t = {
      tsnext : tstmp;
      story  : (tstmp * int * ViewFront.t) list;
    }

    type lt' = {
      ltsnext : Nat.logic;
      lstory  : (Nat.logic * Nat.logic * ViewFront.lt) logic MiniKanren.List.logic;
    }

    type lt = lt' logic

    let empty = { tsnext = 0; story = []; }

    let from_list x =
      let maxts = List.fold_left (fun a (b, _, _) -> max a b) (-1) x in
        { tsnext = maxts + 1; story = x; }

    let (!) = (!!)

    let inj t = !! {
      ltsnext = inj_nat t.tsnext;
      lstory  = MiniKanren.List.inj Cell.inj @@ MiniKanren.List.of_list t.story;
    }

    let prj_msg (lts, lv, lvf) = (prj_nat lts, prj_nat lv, ViewFront.prj lvf)

    let prj lt =
      let lt' = !? lt in {
        tsnext = prj_nat lt'.ltsnext;
        story  = MiniKanren.List.to_list @@ MiniKanren.List.prj (fun lmsg -> prj_msg !?lmsg) lt'.lstory;
      }

    let show t =
      let content = List.fold_left (fun a cell -> a ^ (Cell.show cell)) "" t.story in
        "{" ^ content ^ "}"

    let eq t t' =
      let
        check_exists cell = List.exists (Cell.eq cell) t'.story
      in
        List.for_all check_exists t.story

    let splito t tsnext story =
      (t === !{ ltsnext = tsnext; lstory = story; })

    let next_tstmpo t ts =
      fresh (story)
        (splito t ts story)

    let visible_msgo ts msg b =
      fresh (ts' v vf)
        (msg === !(ts', v, vf))
        (Nat.leo ts ts' b)

    let read_acqo t ts ts' v vf =
      fresh (story story' msg tsnext)
        (splito t tsnext story)
        (MiniKanren.List.filtero (visible_msgo ts) story story')
        (MiniKanren.List.membero story' msg)
        (msg === !(ts', v, vf))

    let write_relo v vf t t' =
      fresh (ts ts' story story')
        (splito t ts story)
        (ts' === !(S ts))
        (story' === !(ts, v, vf) % story)
        (splito t' ts' story')

    let read_acq t ts = run qrs (fun q  r  s  -> read_acqo (inj t) (inj_nat ts) q r s)
                                (fun qs rs ss -> Utils.zip3 (Stream.map prj_nat qs) (Stream.map prj_nat rs) (Stream.map ViewFront.prj ss))

    let write_rel v vf t = run q (fun q  -> write_relo (inj_nat v) (ViewFront.inj vf) (inj t) q)
                                 (fun qs -> prj @@ Utils.excl_answ qs)
  end

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
