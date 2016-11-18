type loc = string
type tstmp = int

type mem_order = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

module Path = 
  struct
    type t = N | L of t | R of t
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

module ThreadState = 
  struct
    type t = {
      curr : ViewFront.t;
    }

    let empty = { curr = ViewFront.empty }
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
