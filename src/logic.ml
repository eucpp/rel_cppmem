open GT
open MiniKanren

module ExprTerm = 
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

    let is_value_term = function
      | Const _ -> true
      | _       -> false

    let is_value_termo t = fresh (n) (t === !!(Const n)) 
  end

module ExprContext =
  struct
    type t  = ExprTerm.t
    type lt = ExprTerm.lt

    @type ('int, 'string, 't, 'c) ac = 
      | Hole
      | BinopL of 'string * 'c * 't
      | BinopR of 'string * 't * 'c
      with gmap, show

    type c  = (int, string, t, c) ac
    type lc = (int logic, string logic, lt, lc) ac logic

    let rec inj_context : c -> lc = fun c ->
      inj (gmap(ac) (inj) (inj) (ExprTerm.inj_term) (inj_context) c)

    let rec prj_context : lc -> c = fun lc ->
      gmap(ac) (prj) (prj) (ExprTerm.prj_term) (prj_context) (prj lc)

    let rec show_context c = show(ac) (show(int)) (show(string)) (ExprTerm.show_term) (show_context) c 

    let rec splito t ct = ExprTerm.(conde [
      fresh (op l r ct' c' t')
         (t === !!(Binop (op, l, r)))
         (ct' === !!(c', t')) 
         (conde [
          ((ct === !!(!!Hole, t))                  &&& (is_value_termo l) &&& (is_value_termo r));
          ((ct === !!(!!(BinopR (op, l, c')), t')) &&& (splito r ct')     &&& (is_value_termo l));
          ((ct === !!(!!(BinopL (op, c', r)), t')) &&& (splito l ct'));
        ]);
      (ct === !!(!!Hole, t));
    ])
  end 

let _ = 
  let module ET = ExprTerm in
  let module EC = ExprContext in
  let e = ET.Binop ("+", ET.Var "x", ET.Const 2) in
    run q (fun q -> EC.splito (ET.inj_term e) q)
          (fun a -> (fun (lc, lt) -> print_string  @@ EC.show_context @@ EC.prj_context lc;
                                     print_string  @@ ", ";
                                     print_endline @@ ET.show_term @@ ET.prj_term lt
                                     ) @@ prj @@ Stream.hd a)
  
