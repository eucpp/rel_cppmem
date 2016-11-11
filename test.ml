open OUnit2

module E  = Lang.Expr
module EC = Lang.ExprContext
module S  = Lang.Stmt
module SC = Lang.StmtContext

module RI = Reduction.Interpreter(EC)

let expr_term_1 = E.Const 1
let expr_term_2 = E.Binop ("+", E.Const 1, E.Const 4)
let expr_term_3 = E.Binop ("*", expr_term_2, expr_term_2)

let check_and_take_first_term = function
  | [hd] -> fst hd
  | _    -> failwith "List of size 1 was expected"

let rules = RI.create [
    "const", Rules.BasicExpr.const;
    "binop", Rules.BasicExpr.binop;
  ]

let test_space_expr_term_1 test_ctxt = 
  let expected = E.Const 1 in
  let actual   = RI.space rules (expr_term_1, EC.default_state) in
    print_int (List.length actual);
    assert_equal 1 (List.length actual);
    assert_equal expected (fst (List.hd actual))


(*
let test_step_expr_2 = 
  assert_equal (E.Const 5) (fst (RI(EC).step (expr_term_1, EC.default_state)))

let test_step_expr_3 =
  let expected = [E.Const 25]
  let actual   = check_and_take_fst (RI(EC).step (expr_term_2, EC.default_state))  
  assert_equal (E.Const 25) 
*)  

let suite =
  "interpreter">:::
    ["test_space_expr_term_1">:: test_space_expr_term_1]

let _ =
  run_test_tt_main suite                       
