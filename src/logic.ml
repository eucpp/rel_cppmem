open GT
open MiniKanren

module ExprTerm = 
  struct
    @type ('int, 'string, 't) at =
      | Const of 'int
      | Var   of 'string
      | Binop of 'string * 't * 't
      | Stuck
      with gmap, eq, show 

    type t  = (int, string, t) at
    type lt = (int logic, string logic, lt) at logic

    let rec inj t = !! (gmap(at) (!!) (!!) (inj) t)

    let rec prj lt = gmap(at) (!?) (!?) (prj) (!? lt)

    let rec show t = GT.show(at) (GT.show(int)) (GT.show(string)) (show) t

    let rec eq t t' = GT.eq(at) (GT.eq(int)) (GT.eq(string)) (eq) t t'

    let is_value_termo t = fresh (n) (t === !!(Const n))

    let not_value_termo t = conde [
      fresh (x)        (t === !!(Var x));
      fresh (op l r)   (t === !!(Binop (op, l, r)));
    ]
  end

module ExprContext =
  struct
    type t  = ExprTerm.t
    type lt = ExprTerm.lt

    @type ('int, 'string, 't, 'c) ac = 
      | Hole
      | BinopL of 'string * 'c * 't
      | BinopR of 'string * 't * 'c
      with gmap, eq, show

    type c  = (int, string, t, c) ac
    type lc = (int logic, string logic, lt, lc) ac logic

    let rec inj c = !! (gmap(ac) (!!) (!!) (ExprTerm.inj) (inj) c)

    let rec prj lc = gmap(ac) (!?) (!?) (ExprTerm.prj) (prj) (!? lc)

    let rec show c = GT.show(ac) (GT.show(int)) (GT.show(string)) (ExprTerm.show) (show) c 

    let rec eq c c' = GT.eq(ac) (GT.eq(int)) (GT.eq(string)) (ExprTerm.eq) (eq) c c'

    let rec splito t ct = ExprTerm.(conde [
      fresh (op l r ct' c' t')
         (t === !!(Binop (op, l, r)))
         (ct' === !!(c', t')) 
         (conde [
           ((ct === !!(!!(BinopL (op, c', r)), t')) &&& (splito l ct')     &&& (not_value_termo l));
           ((ct === !!(!!(BinopR (op, l, c')), t')) &&& (splito r ct')     &&& (not_value_termo r));
           ((ct === !!(!!Hole, t))                  &&& (is_value_termo l) &&& (is_value_termo r));
        ]);
      fresh (x)
        ((t === !!(Var x)) &&& (ct === !!(!!Hole, t)));
      fresh (n)
        ((t === !!(Const n)) &&& (ct === !!(!!Hole, t)));
    ])
  end 

(* let _ =  *)
(*   let module ET = ExprTerm in *)
(*   let module EC = ExprContext in *)
(*   let e = ET.Binop ("+", ET.Var "x", ET.Const 2) in *)
(*     run q (fun q -> EC.splito (ET.inj e) q) *)
(*           (fun a -> (fun (lc, lt) -> print_string  @@ EC.show @@ EC.prj lc; *)
(*                                      print_string  @@ ", "; *)
(*                                      print_endline @@ ET.show @@ ET.prj lt *)
(*                                      ) @@ prj @@ Stream.hd a) *)
  
