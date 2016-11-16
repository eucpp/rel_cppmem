module E  = Lang.Expr
module EC = Lang.ExprContext 

module S  = Lang.Stmt
module SC = Lang.StmtContext

module ST = Lang.State 
module H  = Lang.History
module T  = Lang.Thread
module VF = Lang.ViewFront

module ExprIntpr = Reduction.Interpreter(EC)

module BasicExpr = 
  struct
    type t = EC.t

    type c = EC.c

    type s = EC.s   

    type rresult = EC.rresult

    let get_value = function
      | E.Const n -> n
      (*| Expr.Var x   -> ExprContext.lookup_var s x*)
      | _            -> failwith "Given term is not a value-term" 

    let read_na' c s var =
      let tm           = H.lash_tstmp var s.history in
      let (_, _, v, _) = H.get var tm s.history in
      let thrd         = T.get_thread s.threads (SC.get_path c) in
        if tm = VF.get var thrd.curr
        then EC.Conclusion (c, E.Const v, s)
        else EC.Rewrite (E.Stuck, s)

    let read_na (c, t, s) = 
      match t with
        | E.Var var -> [read_na' c s var]
        | _         -> [EC.Skip]

    let apply_binop op l r =
      let l' = get_value l in
      let r' = get_value r in
        match op with
          | "+" -> E.Const (l' + r')
          | "-" -> E.Const (l' - r')
          | "*" -> E.Const (l' * r')
          | _   -> failwith "Unsupported binary operator"        

    let binop (c, t, s) = 
      match t with
        | E.Binop (op, l, r) -> [EC.Conclusion (c, apply_binop op l r, s)]
        | _                  -> [EC.Skip]  
  end

module BasicStmt = 
  struct 
    type t = EC.t

    type c = EC.c

    type s = EC.s   

    type rresult = EC.rresult

    let expr_rules = ExprIntpr.create [
      "read_na", BasicExpr.read_na;
      "binop"  , BasicExpr.binop;
    ]
    
    let write_na' c s var v = 
      let tm   = H.last_tstmp var s.history in
      let thrd = T.get_thread s.threads (SC.get_path c) in
        if tm = VF.get var thrd.curr
        then 
          let tm'      = tm + 1 in
          let h'       = H.insert var tm' v (VF.create ()) in
          let thrd'    = {thrd with curr = VF.update var tm' thrd.curr} in
          let threads' = T.update_thread s.threads (SC.get_path c) thrd' in
            SC.Conclusion (c, S.Skip, { history = h'; threads = threads'; })
        else 
          SC.Rewrite (S.Stuck, s)

    let write_na (c, t, s) = 
      match t with
        | S.Write (mo, loc, e) when mo = Lang.NA ->
             ExprIntpr.space expr_rules (e, s)
          |> List.map (fun (E.Const v, s') -> write_na' c s' loc v) 
        | _                                      -> [SC.Skip]
             
    let assign' c s el er =
      match (el, er) with
        | (E.Var var, E.Const v) -> write_na' c s var v
        | _                      -> failwith "Bad assignment"

    let assign (c, t, s) = 
      match t with
        | S.Asgn (S.AExpr el, S.AExpr er) ->
              ExprIntpr.space expr_rules (el, s)
           |> List.map (fun (el', s') -> 
                List.map (fun (er', s'') -> assign' c s'' el' er') (ExprIntpr.space expr_rules (er, s')))
           |> List.concat
        
        | _                               -> [SC.Skip]

  end
