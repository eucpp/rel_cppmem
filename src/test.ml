open OUnit2

module E  = Lang.Expr
module EC = Lang.ExprContext
module S  = Lang.Stmt
module SC = Lang.StmtContext

module RI = Reduction.Interpreter(EC)

let check_and_take_first_term = function
  | [hd] -> fst hd
  | _    -> failwith "List of size 1 was expected"

let rules = RI.create [
    (* "const", Rules.BasicExpr.const; *)
    "binop", Rules.BasicExpr.binop;
  ]

let test_space_expr_term expected init test_ctx = 
  let actual = RI.space rules (init, EC.default_state) in
    assert_equal 1 (List.length actual);
    assert_equal expected (fst (List.hd actual))

let expr_term_1 = E.Const 1
let expr_term_2 = E.Binop ("+", E.Const 1, E.Const 4)
let expr_term_3 = E.Binop ("*", expr_term_2, expr_term_2)

let suite =
  "interpreter">::: [
    "test_space_expr_term_1">:: test_space_expr_term (E.Const 1)  expr_term_1;
    "test_space_expr_term_2">:: test_space_expr_term (E.Const 5)  expr_term_2;
    "test_space_expr_term_3">:: test_space_expr_term (E.Const 25) expr_term_3
  ]

let _ =
  run_test_tt_main suite                       
