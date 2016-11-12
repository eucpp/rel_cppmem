module BasicExpr = 
  struct
    module E  = Lang.Expr
    module EC = Lang.ExprContext 

    type t = EC.t

    type c = EC.c

    type s = EC.s   

    type rresult = EC.rresult

    let get_value = function
      | E.Const n -> n
      (*| Expr.Var x   -> ExprContext.lookup_var s x*)
      | _            -> failwith "Given term is not a value-term" 

    let apply_binop op l r =
      let l' = get_value l in
      let r' = get_value r in
        match op with
          | "+" -> E.Const (l' + r')
          | "-" -> E.Const (l' - r')
          | "*" -> E.Const (l' * r')
          | _   -> failwith "Unsupported binary operator"        

    let const (t, s) =
      match t with  
        | E.Const _ -> EC.Conclusion (t, s)
        | _         -> EC.Skip 

    let binop (t, s) = 
      match t with
        | E.Binop (op, l, r) -> EC.Conclusion (apply_binop op l r, s)
        | _                  -> EC.Skip  


  end
