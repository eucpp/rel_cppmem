open OUnit2

module E  = Lang.Expr
module EC = Lang.ExprContext
module S  = Lang.Stmt
module SC = Lang.StmtContext

module RI = Reduction.Interpreter(EC)

module H  = Lang.History
module VF = Lang.ViewFront

let check_and_take_first_term = function
  | [hd] -> fst hd
  | _    -> failwith "List of size 1 was expected"

let rules = RI.create [
    (* "const", Rules.BasicExpr.const; *)
    "binop", Rules.BasicExpr.binop;
  ]

let test_space_expr_term expected init test_ctx = 
  let actual = RI.space rules (init, EC.default_state) in
    assert_equal 1 (List.length actual)  ~printer:string_of_int;
    assert_equal expected (fst (List.hd actual)) 

let expr_term_1 = E.Const 1
let expr_term_2 = E.Binop ("+", E.Const 1, E.Const 4)
let expr_term_3 = E.Binop ("*", expr_term_2, expr_term_2)

let vfront = VF.create ()

let h = 
     H.create ()
  |> H.insert "x" 0 1 vfront
  |> H.insert "x" 1 2 vfront
  |> H.insert "y" 0 0 vfront

let print_hcell (l, t, v, _) = Printf.sprintf "(%s, %d, %d)" l t v

let suite =
  "suite">::: [
    "test_space_expr_term_1">:: test_space_expr_term (E.Const 1)  expr_term_1;
    "test_space_expr_term_2">:: test_space_expr_term (E.Const 5)  expr_term_2;
    "test_space_expr_term_3">:: test_space_expr_term (E.Const 25) expr_term_3;

    "test_history_get_1">:: (fun test_ctxt ->
      assert_equal ("x", 1, 2, vfront) (H.get "x" 0 h) ~printer:print_hcell);

    "test_history_get_2">:: (fun test_ctxt ->
      assert_equal ("y", 0, 0, vfront) (H.get "y" 0 h) ~printer:print_hcell);

    "test_history_get_3">:: (fun test_ctxt ->
      assert_raises Not_found (fun _ -> H.get "y" 1 h));

    "test_history_next_tstmp">:: (fun test_ctxt ->
      assert_equal 2 (H.next_tstmp "x" h) ~printer:string_of_int);
  ]

let _ =
  run_test_tt_main suite
  (* RI.space rules (expr_term_2, EC.default_state) *)
