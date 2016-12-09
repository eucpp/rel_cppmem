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

    let (!) = MiniKanren.inj

    let inj = Utils.inj_assoc (!)  (inj_nat)
    let prj = Utils.prj_assoc (!?) (prj_nat)

    let show = Utils.show_assoc (string_of_loc) (string_of_tstmp) 

    let eq = Utils.eq_assoc (=) (=)

    let geto = Utils.assoco

    let removo = Utils.remove_assoco

    let updateo = Utils.update_assoco

    let get var regs = run q (fun q  -> geto !var (inj regs) q)
                             (fun qs -> prj_nat @@ Utils.excl_answ qs)

    let remove var regs = run q (fun q  -> removo !var (inj regs) q)
                                (fun qs -> prj @@ Utils.excl_answ qs) 

    let update var v regs = run q (fun q  -> updateo !var (inj_nat v) (inj regs) q)
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

    let show t = "Registers: " ^ Registers.show t.regs ^ "\nCurrent viewfront: " ^ ViewFront.show t.curr
    
    let eq t t' = (Registers.eq t.regs t'.regs) && (ViewFront.eq t.curr t'.curr)

    let (!) = (!!)

    let get_regso t regs = 
      fresh (curr)
        (t === !{ lregs = regs; lcurr = curr; })
  
    let set_regso regs' t t' = 
      fresh (regs curr)
        (t  === !!{ lregs = regs;  lcurr = curr; })
        (t' === !!{ lregs = regs'; lcurr = curr; })

    let assign_localo var v t t' = 
      fresh (regs regs')
        (get_regso t regs)
        (Registers.seto var v regs regs')
        (set_regso regs' t t')        

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

    let get_thrd path thrd_tree = run q (fun q  -> get_thrdo (Path.inj path) (inj thrd_tree) q)
                                        (fun qs -> ThreadState.prj @@ Utils.excl_answ qs)

    let update_thrd path thrd thrd_tree = run q (fun q  -> update_thrdo (Path.inj path) (ThreadState.inj thrd) (inj thrd_tree) q)
                                                (fun qs -> prj @@ Utils.excl_answ qs)
  end

module History = 
  struct 
    type t = (loc * tstmp * int * ViewFront.t) list

    let empty = []

    let last_tstmp l h = 
      let _, t, _, _ = List.find (fun (l', _, _, _) -> l = l') h in
        t

    let next_tstmp l h = 
      try
        1 + (last_tstmp l h) 
      with
        | Not_found -> 0
    
    let insert l t v vfront h =
      let (lpart, rpart) = List.partition (fun (l', t', _, _) -> l' < l || t' > t) h in
        lpart @ [(l, t, v, vfront)] @ rpart

    let get l tmin h = List.find (fun (l', t', _, _) -> l = l' && tmin <= t') h
  end

module MemState =
  struct
    type t = {
      thrds : ThreadTree.t;
    }

    type lt' = {
      lthrds : ThreadTree.lt;
    }

    type lt = lt' MiniKanren.logic

    let empty = { thrds = ThreadTree.empty; }

    let inj t = !! { lthrds = ThreadTree.inj t.thrds; }

    let prj lt = 
      let lt' = !?lt in
      { thrds = ThreadTree.prj lt'.lthrds; }

    let sep = "-------------------------------------------------------------"

    let show t = "Threads\n" ^ sep ^ "\n" ^ ThreadTree.show t.thrds
    
    let eq t t' = ThreadTree.eq t.thrds t'.thrds

    let (!) = (!!)

    let get_thrd_treeo t tree = 
      (t === !{lthrds = tree;})

    let set_thrd_treeo tree t t' = 
      (t' === !{lthrds = tree;})
 
    let get_thrdo path t thrd =
      fresh (thrd_tree)
        (get_thrd_treeo t thrd_tree)
        (ThreadTree.get_thrdo path thrd_tree thrd) 
      
    let update_thrdo path thrd t t' = 
      fresh (thrd_tree thrd_tree')
        (get_thrd_treeo t thrd_tree)
        (ThreadTree.update_thrdo path thrd thrd_tree thrd_tree')
        (set_thrd_treeo thrd_tree' t t')
 
    let assign_localo path x n t t' = 
      fresh (thrd thrd')
        (get_thrdo path t thrd)
        (ThreadState.assign_localo x n thrd thrd')
        (update_thrdo path thrd' t t')

    let assign_local path x n t = run q (fun q  -> assign_localo (Path.inj path) !x (inj_nat n) (inj t) q)
                                        (fun qs -> prj @@ Utils.excl_answ qs)
  end
