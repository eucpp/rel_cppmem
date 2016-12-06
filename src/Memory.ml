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
    type t = N | L of t | R of t
  end

module Registers = 
  struct
    type t   = (string * int) list
    type lt' = ((string logic * Nat.logic) logic, lt' logic) llist   
    type lt  = lt' logic
    
    let empty = []

    let (!) = MiniKanren.inj

    let prj_pair lp = (fun (k, v) -> (!?k, Nat.to_int (Nat.prj v))) (!?lp)

    let inj l = MiniKanren.List.inj (fun (k, v) -> !(!k, Nat.inj (Nat.of_int v))) @@ MiniKanren.List.of_list l
    let prj l = MiniKanren.List.to_list @@ MiniKanren.List.prj prj_pair l

    let show = List.fold_left (fun ac (k, v) -> ac ^ " {" ^ k ^ ": " ^ string_of_int v ^ "}; ") ""

    let eq regs regs' = 
      let 
        check_reg reg = List.exists ((=) reg) regs'
      in
        List.for_all check_reg regs

    let make_reg var v = !(var, v)

    let key_eq k p b =
      fresh (k' v')
        (p === !(k', v'))
        (conde [
          ((k === k') &&& (b === !true));
          ((k =/= k') &&& (b === !false)) 
        ])

    let geto var regs v = 
      fresh (opt) 
        (MiniKanren.List.lookupo (key_eq var) regs opt)
        (opt === !(Some !(var, v)))

    let seto var v regs regs'' = 
      fresh (regs')
        (MiniKanren.List.filtero (key_eq var) regs regs')
        (regs'' === (make_reg var v) % regs)

    let get var regs = run q (fun q  -> geto !var (inj regs) q)
                             (fun qs -> prj_nat @@ Utils.excl_answ qs)

    let set var v regs = run q (fun q  -> seto !var (inj_nat v) (inj regs) q)
                               (fun qs -> (prj @@ Utils.excl_answ qs)) 
  end

module ViewFront = 
  struct
    type t = (loc * tstmp) list

    let empty = []

    let get l vfront = List.assoc l vfront
    
    let update l t vfront =
      let vfront' = List.remove_assoc l vfront in
        (l, t)::vfront' 
  end

module ThreadState :
  sig
    type t = {
      regs : Registers.t
      (* curr : ViewFront.t; *)
    }

    type lt' = {
      lregs : Registers.lt;
    }

    type lt = lt' MiniKanren.logic

    let empty = { regs = Registers.empty; }

    let inj t  = { lregs = Registers.inj t.regs; }
    
    let prj lt = 
      let lt' = !?lt in
      { regs = Registers.prj lt'.regs; }

    let show t = "Registers: " ^ Registers.show t.regs
    
    let eq t t' = Registers.eq t.regs t'.regs
  end

module ThreadTree : 
  struct
    @type ('a 't) at = Leaf of 'a | Node of 't * 't with gmap
 
    type t   = (ThreadState.t, t) at
    type lt' = (ThreadState.lt, lt' logic) at
    type lt  = lt' logic

    let empty = Leaf ThreadState.empty 

    let rec inj t  = !! (gmap(at) (ThreadState.inj) (inj) t)
    let rec prj lt = gmap(at) (ThreadState.prj) (prj) (!?lt)

    val show : t -> string
    val eq : t -> t -> bool

    val get_thrdo    : Path.lt -> lt -> ThreadState.lt -> MiniKanren.goal
    val update_thrdo : Path.lt -> ThreadState.lt -> lt -> lt -> MiniKanren.goal      

    val get_thrd    : Path.t -> t -> ThreadState.t
    val update_thrd : Path.t -> ThreadState.t -> t -> t
  end

module ThreadTree = 
  struct
    type t = Leaf of ThreadState.t | Node of t * t

    let empty = Leaf ThreadState.empty

    let rec get_thread t p = 
       match (t, p) with
         | Node (l, _), Path.L p' -> get_thread l p'
         | Node (_, r), Path.R p' -> get_thread r p'
         | Leaf thrd  , Path.N    -> thrd
         | _          , _         -> failwith "Incorrect path"

    let rec update_thread t p thrd = 
       match (t, p) with
         | Node (l, r), Path.L p' -> Node (update_thread l p' thrd, r)
         | Node (l, r), Path.R p' -> Node (l, update_thread r p' thrd)
         | Leaf _     , Path.N    -> Leaf thrd
         | _          , _         -> failwith "Incorrect path"       
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

module StateST = 
  struct 
    type t = {
      history : History.t;
      thread  : ThreadState.t;
    }

    let empty = { history = History.empty; thread = ThreadState.empty; }
  end

module StateMT = 
  struct 
    type t = {
      history : History.t;
      tree    : ThreadTree.t;
    }

    let empty = { history = History.empty; tree = ThreadTree.empty; }
  end 
