open GT
open MiniKanren

type loc = string
type tstmp = int

type mem_order = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

let string_of_loc = fun x -> x
let string_of_tstmp = string_of_int

let string_of_mo = function
  | SC      -> "sc"
  | ACQ     -> "acq"
  | REL     -> "rel"
  | ACQ_REL -> "acq_rel"
  | CON     -> "con"
  | RLX     -> "rlx"
  | NA      -> "na"  

module Path = 
  struct
    type 'a at = N | L of 'a | R of 'a

    type t  = t  at
    type lt = lt at MiniKanren.logic

    let (!) = (!!)

    let rec inj = function
    | N -> !N
    | L p -> !(L (inj p))
    | R p -> !(R (inj p))


    let rec prj lp = 
      let p = !?lp in
        match p with
        | N -> N
        | L lp' -> L (prj lp')
        | R lp' -> R (prj lp')
 
  end

module Registers = 
  struct
    type t   = (string * int) list
    type lt' = ((string logic * Nat.logic) logic, lt' logic) llist   
    type lt  = lt' logic
    
    let empty = []

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

module ViewFront =
  struct
    type t   = (loc * tstmp) list
    type lt' = ((loc logic * Nat.logic) logic, lt' logic) llist   
    type lt  = lt' logic

    let empty = []

    let inj = Utils.inj_assoc (!!)  (inj_nat)
    let prj = Utils.prj_assoc (!?) (prj_nat)

    let show = Utils.show_assoc (string_of_loc) (string_of_tstmp) 

    let eq = Utils.eq_assoc (=) (=)

    let from_assoc assoc = assoc

    let (!) = MiniKanren.inj 
 
    let geto = Utils.assoco

    let updateo = Utils.update_assoco

    (* let join_loco lts vf vf' =  *)
    (*   fresh (l ts ts' opt) *)
    (*     (lts === !(l, ts)) *)
    (*     (MiniKanren.List.lookupo (Utils.key_eqo l) vf opt) *)
    (*     (conde [ *)
    (*       (opt === !None) &&& *)
    (*       (vf' === lts % vf); *)

    (*       (opt === !(Some !(l, ts'))) &&& *)
    (*       (conde [ *)
    (*         Nat.(ts <= ts') &&& (vf' === vf); *)
    (*         Nat.(ts >  ts') &&& (updateo l ts vf vf'); *)
    (*       ]); *)
    (*     ]) *)

    let rec join_loco lts vf vf' = 
      fresh (l l' ts ts')
        (lts === !(l, ts))
        (conde [
          (vf === !MiniKanren.Nil) &&& (vf' === lts % vf);

          fresh (tl tl')
            (vf === !(l', ts') % tl)
            (conde [
              (l === l') &&& conde [
                Nat.(ts <= ts') &&& (vf' === vf);
                Nat.(ts >  ts') &&& (vf' === !(l, ts) % tl);
              ];

              (l =/= l') &&& (vf' === !(l', ts') % tl') &&& (join_loco lts tl tl');
            ])
        ])

    let joino t t' joined = 
      MiniKanren.List.foldro join_loco t t' joined

    let get var vf = run q (fun q  -> geto !var (inj vf) q)
                           (fun qs -> prj_nat @@ Utils.excl_answ qs)

    let update var v vf = run q (fun q  -> updateo !var (inj_nat v) (inj vf) q)
                                (fun qs -> prj @@ Utils.excl_answ qs) 

    let join vf vf' = run q (fun q  -> joino (inj vf) (inj vf') q)
                            (fun qs -> prj @@ Utils.excl_answ qs) 

  end

module ThreadState =
  struct
    type t = {
      regs : Registers.t;
      curr : ViewFront.t;
    }

    type lt' = {
      lregs : Registers.lt;
      lcurr : ViewFront.lt;
    }

    type lt = lt' logic

    let empty = { regs = Registers.empty; curr = ViewFront.empty; }

    let inj t  = !! { lregs = Registers.inj t.regs; lcurr = ViewFront.inj t.curr; }
    
    let prj lt = 
      let lt' = !?lt in
      { regs = Registers.prj lt'.lregs; 
        curr = ViewFront.prj lt'.lcurr; }

    let show t = "Registers: " ^ Registers.show t.regs ^ "\nCurrent viewfront: " ^ ViewFront.show t.curr ^ "\n"
    
    let eq t t' = (Registers.eq t.regs t'.regs) && (ViewFront.eq t.curr t'.curr)

    let (!) = (!!)

    let splito t regs curr = 
      (t === !{lregs = regs; lcurr = curr; }) 

    let get_localo t var v = 
      fresh (regs curr)
        (splito t regs curr)
        (Registers.geto var regs v) 

    let assign_localo var v t t' = 
      fresh (regs regs' curr)
        (splito t regs curr)
        (Registers.seto var v regs regs')
        (splito t' regs' curr)

    let get_tstmpo t l ts = 
      fresh (regs curr)
        (splito t regs curr)
        (ViewFront.geto l curr ts)

    let update_tstmpo l ts t t' curr' = 
      fresh (regs curr) 
        (splito t  regs curr)
        (ViewFront.updateo l ts curr curr')
        (splito t' regs curr')

    let join_viewfronto vf t t' = 
      fresh (regs curr curr')
        (splito t regs curr)
        (ViewFront.joino vf curr curr')
        (splito t' regs curr')

    let spawno t spwn spwn' = 
      fresh (regs curr)
        (splito t regs curr)
        (splito spwn (Registers.inj Registers.empty) curr)
        (spwn' === spwn)

    let joino t t' joined = 
      fresh (regs regs' curr curr' curr'')
        (splito t  regs  curr)
        (splito t' regs' curr')
        (ViewFront.joino curr curr' curr'')
        (splito joined (Registers.inj Registers.empty) curr'')      

  end

module ThreadTree =
  struct
    @type ('a, 't) at = Leaf of 'a | Node of 't * 't with gmap
 
    type t   = (ThreadState.t, t) at
    type lt' = (ThreadState.lt, lt' logic) at
    type lt  = lt' logic

    let empty = Leaf ThreadState.empty 

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

    let (!) = (!!)

    let from_assoc assoc = assoc

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
        (Utils.assoco l t story)
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

module MemState =
  struct
    type t = {
      thrds : ThreadTree.t;
      story : MemStory.t;
    }

    type lt' = {
      lthrds : ThreadTree.lt;
      lstory : MemStory.lt;
    }

    type lt = lt' MiniKanren.logic

    let empty = { thrds = ThreadTree.empty; story = MemStory.empty }

    let inj t = !! { lthrds = ThreadTree.inj t.thrds; lstory = MemStory.inj t.story; }

    let prj lt = 
      let lt' = !?lt in
      { thrds = ThreadTree.prj lt'.lthrds;
        story = MemStory.prj lt'.lstory; }

    let sep = "-------------------------------------------------------------"

    let show t = Printf.sprintf "Threads:\n%s \n%s \nMemory:\n%s \n%s" sep (ThreadTree.show t.thrds) sep (MemStory.show t.story)
    
    let eq t t' = (ThreadTree.eq t.thrds t'.thrds) && (MemStory.eq t.story t'.story)

    let (!) = (!!)

    let splito t thrd_tree story = 
      (t === !{ lthrds = thrd_tree; lstory = story; })
 
    let get_thrdo path t thrd =
      fresh (thrd_tree h)
        (splito t thrd_tree h)
        (ThreadTree.get_thrdo path thrd_tree thrd) 
      
    let update_thrdo path thrd t t' = 
      fresh (thrd_tree thrd_tree' h)
        (splito t thrd_tree h)
        (ThreadTree.update_thrdo path thrd thrd_tree thrd_tree')
        (splito t' thrd_tree' h)
 
    let assign_localo path x n t t' = 
      fresh (thrd thrd')
        (get_thrdo path t thrd)
        (ThreadState.assign_localo x n thrd thrd')
        (update_thrdo path thrd' t t')

    let read_acqo path l v t t' = 
      fresh (thrd_tree thrd_tree' story thrd thrd' ts ts' vf)
        (splito t thrd_tree story)
        (ThreadTree.get_thrdo path thrd_tree thrd)
        (ThreadState.get_tstmpo thrd l ts)
        (MemStory.read_acqo story l ts ts' v vf)
        (ThreadState.join_viewfronto vf thrd thrd')
        (ThreadTree.update_thrdo path thrd' thrd_tree thrd_tree')
        (splito t' thrd_tree' story)

    let write_relo path l v t t' = 
      fresh (thrd_tree thrd_tree' story story' thrd thrd' ts vf)
        (splito t thrd_tree story)
        (MemStory.next_tstmpo story l ts)
        (ThreadTree.get_thrdo path thrd_tree thrd)
        (ThreadState.update_tstmpo l ts thrd thrd' vf)
        (MemStory.write_relo l v vf story story')
        (ThreadTree.update_thrdo path thrd' thrd_tree thrd_tree')
        (splito t' thrd_tree' story')

    let spawn_thrdo path t t' = 
      fresh (thrd_tree thrd_tree' h)
        (splito t thrd_tree h)
        (ThreadTree.spawn_thrdo path thrd_tree thrd_tree')
        (splito t' thrd_tree' h)

    let join_thrdo path t t' = 
      fresh (thrd_tree thrd_tree' h)
        (splito t thrd_tree h)
        (ThreadTree.join_thrdo path thrd_tree thrd_tree')
        (splito t' thrd_tree' h)

    let get_thrd path t = run q (fun q  -> get_thrdo (Path.inj path) (inj t) q)
                                (fun qs -> ThreadState.prj @@ Utils.excl_answ qs)

    let assign_local path x n t = run q (fun q  -> assign_localo (Path.inj path) !x (inj_nat n) (inj t) q)
                                        (fun qs -> prj @@ Utils.excl_answ qs)

    let read_acq path l t = run qr (fun q  r  -> read_acqo (Path.inj path) (!l) q (inj t) r)
                                   (fun qs rs -> Stream.zip (Stream.map prj_nat qs) (Stream.map prj rs))

    let write_rel path l v t = run q (fun q  -> write_relo (Path.inj path) (!l) (inj_nat v) (inj t) q)
                                     (fun qs -> prj @@ Utils.excl_answ qs)
  end
