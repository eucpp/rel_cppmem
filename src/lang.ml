type mem_order = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

type loc = string
type tstmp = int

type path = N | L of path | R of path

module ViewFront = 
  struct
    type t = (loc * tstmp) list

    let create = fun _ -> []

    let get l vfront = List.assoc l vfront
    
    let update l t vfront =
      let vfront' = List.remove_assoc l vfront in
        (l, t)::vfront' 
  end

module Thread = 
  struct
    type t = {
      curr : ViewFront.t;
    }

    type tree = Leaf of t | Node of tree * tree

    let create _ = { curr = ViewFront.create () }

    let create_tree _ = Leaf (create ())

    let rec get_thread t p = 
       match (t, p) with
         | Node (l, _), L p' -> get_thread l p'
         | Node (_, r), R p' -> get_thread r p'
         | Leaf thrd  , N    -> thrd
         | _          , _    -> failwith "Incorrect path"

    let rec update_thread t p thrd = 
       match (t, p) with
         | Node (l, r), L p' -> Node (update_thread l p' thrd, r)
         | Node (l, r), R p' -> Node (l, update_thread r p' thrd)
         | Leaf _     , N    -> Leaf thrd
         | _          , _    -> failwith "Incorrect path"       
       
  end

module History = 
  struct 
    type t = (loc * tstmp * int * ViewFront.t) list

    let create = fun _ -> []

    let last_tstmp l h = 
      let _, t, _, _ = List.find (fun (l', _, _, _) -> l = l') h in
        t

    let next_tstmp l h = 
      try
        1 + (last_tstmp l h) 
      with
        | Not_found -> 0
    
    let insert l t v vfront h =
      let (lpart, rpart) = List.partition (fun (l', t', _, _) -> l' <= l && t' > t) h in
        lpart @ [(l, t, v, vfront)] @ rpart

    let get l tmin h = List.find (fun (l', t', _, _) -> l = l' && tmin <= t') h
  end

module State = 
  struct 
    type t = {
      history : History.t;
      threads : Thread.tree;
    }

    let create _ = { history = History.create (); threads = Thread.create_tree (); }
  end 

module Expr = 
  struct
    type t = 
      | Const    of int
      | Var      of string
      | Binop    of string * t * t
      | Stuck

    let is_value = function
      | Const _ -> true
      | _       -> false

    let is_var = function
      | Var _ -> true
      | _     -> false
  end

module ExprContext = 
  struct
    type t = Expr.t

    type c =
      | Hole
      | BinopL   of string * c * t
      | BinopR   of string * t * c 

    type s = State.t

    type rresult =
      | Conclusion of c * t * s
      | Rewrite    of t * s 
      | Skip

    type rule = (c * t * s -> rresult list)

    let default_state = State.create ()

    let rec split = 
      let module E = Expr in 
        function
          | E.Binop (op, x, y) as t when E.is_value x && E.is_value y -> [Hole, t]
          | E.Binop (op, x, y)      when E.is_value x                 ->
              List.map (fun (c, t) -> BinopR (op, x, c), t) (split y)
          | E.Binop (op, x, y)                                        ->
              List.map (fun (c, t) -> BinopL (op, c, y), t) (split x)
          | t -> [Hole, t]

    let rec plug (c, t) = 
      let module E = Expr in 
        match c with
          | Hole                        -> t
          | BinopL (op, c', t')         -> E.Binop (op, plug (c', t), t')
          | BinopR (op, t', c')         -> E.Binop (op, t', plug (c', t))
  end

module Stmt = 
  struct
    type t =
      | AExpr    of Expr.t
      | Asgn     of t * t
      | If       of Expr.t * t * t
      | Repeat   of t
      | Read     of mem_order * loc
      | Write    of mem_order * loc * Expr.t
      | Cas      of mem_order * mem_order * loc * Expr.t * Expr.t
      | Seq      of t * t
      | Spw      of t * t
      | Par      of t * t
      | Skip
      | Stuck

    let is_value = function
      | AExpr e   -> Expr.is_value e 
      | _         -> false

    let is_var = function
      | AExpr e   -> Expr.is_var e
      | _         -> false
  end

module StmtContext = 
  struct
    type t = Stmt.t

    type c =
      | Hole
      | AsgnL    of c * t
      | AsgnR    of t * c
      | Repeat   of c
      | Seq      of c * t
      | ParL     of c * t
      | ParR     of t * c

    type s = State.t

    type rresult =
      | Conclusion of c * t * s 
      | Rewrite    of t * s
      | Skip

    type rule = (c * t * s -> rresult list)

    let default_state = State.create ()

    let rec split = 
      let module E = Expr in
      let module S = Stmt in 
        function
          | S.Asgn    (x, y) as t when S.is_var x && S.is_value y -> [Hole, t]
          | S.Asgn    (x, y) as t when S.is_var x                 ->
              List.map (fun (c, t) -> AsgnR (x, c), t) (split y)
          | S.Asgn    (x, y)                                      ->
              List.map (fun (c, t) -> AsgnL (c, y), t) (split x)

          | S.Repeat x as t when S.is_value x    -> [Hole, t]
          | S.Repeat x                           ->
              List.map (fun (c, t) -> Repeat c, t) (split x)

          | S.Seq (S.Skip, y) as t -> [Hole, t]
          | S.Seq (x, y)           ->
              List.map (fun (c, t) -> Seq (c, y), t) (split x)

          | S.Par  (l, r) as t ->
             let lcontexts = List.map (fun (c, t) -> ParL (c, r), t) (split l) in
             let rcontexts = List.map (fun (c, t) -> ParR (l, c), t) (split r) in
             (match S.is_value l, S.is_value r with
                | false, false -> lcontexts @ rcontexts
                | _    , false -> rcontexts
                | false, _     -> lcontexts
                | _            -> [Hole, t]
             )

          | t -> [Hole, t]

    let rec plug (c, t) =
      let module S = Stmt in
      let module EC = ExprContext in
        match c with
          | Hole                         -> t
          
          | AsgnL  (c', t')              -> S.Asgn (plug (c', t), t')
          | AsgnR  (t', c')              -> S.Asgn (t', plug (c', t))

          | Repeat c'                    -> S.Repeat (plug (c', t))

          | Seq  (c', t')                -> S.Seq (plug (c', t), t')
          | ParL (c', t')                -> S.Par (plug (c', t), t')
          | ParR (t', c')                -> S.Par (t', plug (c', t))

    let rec get_path = function
      | Hole -> N

      | AsgnL   (c, _)
      | AsgnR   (_, c)
      | Seq     (c, _)
      | Repeat c
        -> get_path c

      | ParL    (c, _) -> L (get_path c)
      | ParR    (_, c) -> R (get_path c) 
  end
