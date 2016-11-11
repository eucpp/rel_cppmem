open OUnit2

open Lang
open Reduction

let expr_term_1 = Expr.Const 1
let expr_term_2 = Expr.Binop "+" (Expr.Const 1) (Expr.Const 4)
let expr_term_3 = Expr.Binop "*" expr_term_1 expr_term_1

let check_and_take_fst = function
  [hd] -> hd
  _    -> failwith "List of size 1 was expected"

let test_space_expr_term_1 = 
  let expected = Expr.Const 1 in
  let actual   = check_and_take_fst (Interpreter(ExprC).space (expr_term_1, ExprC.default_state)) in
    assert_equal expected actual


(*
let test_step_expr_2 = 
  assert_equal (Expr.Const 5) (fst (RI(ExprC).step (expr_term_1, ExprC.default_state)))

let test_step_expr_3 =
  let expected = [Expr.Const 25]
  let actual   = check_and_take_fst (RI(ExprC).step (expr_term_2, ExprC.default_state))  
  assert_equal (Expr.Const 25) 
*)  

let suite =
  "interpreter">:::
    ["test getPath">:: test_getPath]
;;

let _ =
  run_test_tt_main suite
;;                       
