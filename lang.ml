type mem_order = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

type loc = Loc of string

module Expr = 
  struct
    type t = 
      | Const    of int
      | Var      of string
      | Binop    of string * t * t

    let is_value = function
      | Const _
      | Var _
          -> true
      | _ -> false
  end

module ExprContext = 
  struct
    type t = Expr.t

    type c =
      | Hole
      | BinopL   of string * c * t
      | BinopR   of string * t * c 

    type s = unit

    type rresult = 
      | Skip
      | Conclusion of t * s

    type rule = (t * s -> rresult)

    let default_state = ()

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
      | Stuck

    let extract_expr = function
      | AExpr e -> e
      | _       -> failwith "Given statement is not an expression"

    let is_value = function
      | AExpr e   -> Expr.is_value e 
      | _         -> false
  end

module StmtContext = 
  struct
    type t = Stmt.t

    type c =
      | Hole
      | AExprC   of ExprContext.c
      | AsgnL    of c * t
      | AsgnR    of t * c
      | IfC      of ExprContext.c * t * t
      | RepeatC  of c
      | WriteC   of mem_order * loc * ExprContext.c
      | CasE     of mem_order * mem_order * loc * ExprContext.c * Expr.t
      | CasD     of mem_order * mem_order * loc * Expr.t * ExprContext.c
      | SeqC     of c * t
      | ParL     of c * t
      | ParR     of t * c

    type s = 
      | Empty

    type rresult = 
      | Skip
      | Conclusion of t * s

    type rule = (t * s -> rresult)

    let rec split = 
      let module S = Stmt in 
        function
          | S.AExpr e as t when Expr.is_value e -> [Hole, t]
          | S.AExpr e                           ->
              List.map (fun (c, t) -> AExprC c, S.AExpr t) (ExprContext.split e) 

          | S.Asgn    (x, y) as t when Stmt.is_value x && Stmt.is_value y -> [Hole, t]
          | S.Asgn    (x, y) as t when Stmt.is_value x                    ->
              List.map (fun (c, t) -> AsgnR (x, c), t) (split y)
          | S.Asgn    (x, y)                                              ->
              List.map (fun (c, t) -> AsgnL (c, y), t) (split x)

          | S.If (e, x, y) as t when Expr.is_value e -> [Hole, t]
          | S.If (e, x, y)                           ->
              List.map (fun (c, t) -> IfC (c, x, y), S.AExpr t) (ExprContext.split e)

          | S.Repeat x as t when Stmt.is_value x -> [Hole, t]
          | S.Repeat x                           ->
              List.map (fun (c, t) -> RepeatC c, t) (split x)

          | S.Write (mo, loc, x) as t when Expr.is_value x -> [Hole, t]
          | S.Write (mo, loc, x)                           ->
              List.map (fun (c, t) -> WriteC (mo, loc, c), S.AExpr t) (ExprContext.split x)

          | S.Cas (_, _, _, x, y) as t       when Expr.is_value x && Expr.is_value y -> [Hole, t]
          | S.Cas (smo, fmo, loc, x, y)      when Expr.is_value x                    ->
              List.map (fun (c, t) -> CasD (smo, fmo, loc, x, c), S.AExpr t) (ExprContext.split y)
          | S.Cas (smo, fmo, loc, x, y)                                              ->
              List.map (fun (c, t) -> CasE (smo, fmo, loc, c, y), S.AExpr t) (ExprContext.split x)

          | S.Seq (x, y) ->
              List.map (fun (c, t) -> SeqC (c, y), t) (split x)

          | S.Par  (l, r) as t ->
             let lcontexts = List.map (fun (c, t) -> ParL (c, r), t) (split l) in
             let rcontexts = List.map (fun (c, t) -> ParR (l, c), t) (split r) in
             (match Stmt.is_value l, Stmt.is_value r with
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
          | AExprC c'                    -> S.AExpr (EC.plug (c', S.extract_expr t))

          | AsgnL  (c', t')              -> S.Asgn (plug (c', t), t')
          | AsgnR  (t', c')              -> S.Asgn (t', plug (c', t))

          | WriteC (mo, loc, c')         -> S.Write (mo, loc, EC.plug (c', S.extract_expr t))
          | CasE (smo, fmo, loc, c', t') -> S.Cas (smo, fmo, loc, EC.plug (c', S.extract_expr t), t')
          | CasD (smo, fmo, loc, t', c') -> S.Cas (smo, fmo, loc, t', EC.plug (c', S.extract_expr t))

          | SeqC (c', t')                -> S.Seq (plug (c', t), t')
          | ParL (c', t')                -> S.Par (plug (c', t), t')
          | ParR (t', c')                -> S.Par (t', plug (c', t))
        end
