type mem_order = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

type loc = Loc of string

type binop = Add | Sub | Mult | Div
                    
type stmt =
  | Const    of int
  | Var      of string
  | Binop    of binop * stmt * stmt
  | Asgn     of stmt * stmt
                                 
  | Read     of mem_order * loc
  | Write    of mem_order * loc * stmt
  | Cas      of mem_order * mem_order * loc * stmt * stmt

  | Seq      of stmt * stmt
  | Par      of stmt * stmt
  | Stuck                         

type context =
  | Hole

  | BinopL   of binop * context * stmt
  | BinopR   of binop * stmt * context
  | AsgnL    of context * stmt
  | AsgnR    of stmt * context

  | WriteC   of mem_order * loc * context
  | CasE     of mem_order * mem_order * loc * context * stmt
  | CasD     of mem_order * mem_order * loc * stmt * context                                                          

  | SeqC     of context * stmt
  | ParL     of context * stmt
  | ParR     of stmt * context                            

type path = N | L of path | R of path

let rec getPath : context -> path = function
  | Hole -> N

  | BinopL (_, c, _)
  | BinopR (_, _, c)
  | AsgnL  (c, _)
  | AsgnR  (_, c)
  | Seq    (c, _)
      -> getPath c

  | ParL (c, _) -> L (getPath c)
  | ParR (_, c) -> R (getPath c)                     

let isValue : stmt -> bool = function
  | Const _
  | Var   _
      -> true
  | _ -> false
           
let rec getContexts : stmt -> (context * stmt) list = function
  | Binop   (op, x, y) as t when isValue x && isValue y -> [Hole, t]
  | Binop   (op, x, y)      when isValue x              ->
      List.map (fun (c, t) -> BinopR (op, x, c), t) (getContexts y)
  | Binop   (op, x, y)                                  ->
      List.map (fun (c, t) -> BinopL (op, c, y), t) (getContexts x)

  | Asgn    (x, y) as t when isValue x && isValue y -> [Hole, t]
  | Asgn    (x, y) as t when isValue x              ->
      List.map (fun (c, t) -> AsgnR (x, c), t) (getContexts y)
  | Asgn    (x, y)                                  ->
      List.map (fun (c, t) -> AsgnL (c, y), t) (getContexts x)

  | Write (mo, loc, x) as t when isValue x -> [Hole, t]
  | Write (mo, loc, x)
      List.map (fun (c, t) -> WriteC (mo, loc, c), t) (getContexts x)

  | Cas (_, _, _, x, y) as t       when isValue x && isValue y -> [Hole, t]
  | Cas (smo, fmo, loc, x, y)      when isValue x              ->
      List.map (fun (c, t) -> CasD (smo, fmo, loc, x, c), t) (getContexts y)
  | Cas (smo, fmo, loc, x, y)                                  ->
      List.map (fun (c, t) -> CasE (smo, fmo, loc, c, y), t) (getContexts x)
               
  | Seq (x, y) ->
      List.map (fun (c, t) -> SeqC (c, y), t) (getContexts x)
               
  | Par  (l, r) as t ->
     let lcontexts = List.map (fun (c, t) -> ParL (c, r), t) (getContexts l) in
     let rcontexts = List.map (fun (c, t) -> ParR (l, c), t) (getContexts r) in
     (match isValue l, isValue r with
        | false, false -> lcontexts @ rcontexts
        | _    , false -> rcontexts
        | false, _     -> lcontexts
        | _            -> [Hole, t]
     )
     
  | t -> [Hole, t]
                                           
let rec applyContext (c : context, t : stmt) : stmt =
  match c with
  | Hole                         -> t
  | BinopL (bop, c', t')         -> Binop (bop, applyContext (c', t), t')
  | BinopR (bop, t', c')         -> Binop (bop, t', applyContext (c', t))
  | AsgnL  (c', t')              -> Asgn (applyContext (c', t), t')
  | AsgnR  (t', c')              -> Asgn (t', applyContext (c', t))
  | WriteC (mo, loc, c')         -> Write (mo, loc, applyContext (c', t))
  | CasE (smo, fmo, loc, c', t') -> Cas (smo, fmo, loc, applyContext (c', t), t')
  | CasD (smo, fmo, loc, t', c') -> Cas (smo, fmo, loc, t', applyContext (c', t))
  | SeqC (c', t')                -> Seq (applyContext (c', t), t')
  | ParL (c', t')                -> Par (applyContext (c', t), t')
  | ParR (t', c')                -> Par (t', applyContext (c', t))                                        
           
let t = Par (Binop (Mult, Const 2, Const 42), Binop (Add, Const 0, Const 2));;

let ctxs = getContexts t;;            
                                                                           
let _ =
  print_string (contextsToString ctxs);
  print_newline;
