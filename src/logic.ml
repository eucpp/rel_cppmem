open GT
open MiniKanren

@type ('int, 'string, 't, 'c) ac = 
      | Hole
      | BinopL of 'string * 'c * 't
      | BinopR of 'string * 't * 'c
      with gmap, show

module ExprContext =
  struct
    @type ('int, 'string, 't) at =
      | Const of 'int
      | Var   of 'string
      | Binop of 'string * 't * 't
      | Stuck
      with gmap, show 

    type t  = (int, string, t) at
    type lt = (int logic, string logic, lt) at logic

    let rec inj_term : t -> lt = fun t -> 
      inj (gmap(at) (inj) (inj) (inj_term) t)

    let rec prj_term : lt -> t = fun lt ->
      gmap(at) (prj) (prj) (prj_term) (prj lt)

    let rec show_term t = show(at) (show(int)) (show(string)) (show_term) t  

    

    type c  = (int, string, t, c) ac
    type lc = (int logic, string logic, lt, lc) ac logic

    let rec inj_context : c -> lc = fun c ->
      inj (gmap(ac) (inj) (inj) (inj_term) (inj_context) c)

    let rec prj_context : lc -> c = fun lc ->
      gmap(ac) (prj) (prj) (prj_term) (prj_context) (prj lc)

    let rec show_context c = show(ac) (show(int)) (show(string)) (show_term) (show_context) c 

    let rec splito t ct = conde [
      (fresh (n) ((t === !!(Const n)) &&& (ct === !!(!!Hole, t))));
      (fresh (x) ((t === !!(Var x))   &&& (ct === !!(!!Hole, t))))
    ]
  end 

let _ = 
  let module EC = ExprContext in
  let e = EC.Var "x" in
    run q (fun q -> EC.splito (EC.inj_term e) q)
          (fun a -> (fun (lc, lt) -> print_string @@ show @@ EC.prj_context lc;
                                     print_string @@ show @@ EC.prj_term lt
                                     ) @@ Stream.hd a)
  
