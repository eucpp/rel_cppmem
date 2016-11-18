open OUnit2

module E  = Lang.Expr
module EC = Lang.ExprContext
module S  = Lang.Stmt
module SC = Lang.StmtContext

module ExprIntpr = Reduction.Interpreter(EC)
module StmtIntpr = Reduction.Interpreter(SC)

module H  = Lang.History
module T  = Lang.Thread
module VF = Lang.ViewFront
module ST = Lang.State

let rec string_of_expr = function
  | E.Const c            -> string_of_int c
  | E.Var x              -> x
  | E.Binop (op, el, er) -> "(" ^ (string_of_expr el) ^ op ^ (string_of_expr er) ^ ")"
  | E.Stuck              -> "Stuck"

let string_of_expr_cfg (t, s) = string_of_expr t
                                                                                     
let check_and_take_first_term = function
  | [hd] -> fst hd
  | _    -> failwith "List of size 1 was expected"

let expr_rules = ExprIntpr.create [
    (* "const", Expr_Rules.BasicExpr.const; *)
    "var"  , Rules.BasicExpr.var;            
    "binop", Rules.BasicExpr.binop;
  ]

let stmt_rules = StmtIntpr.create [
    "read_na" , Rules.BasicStmt.read_na;
    "write_na", Rules.BasicStmt.write_na;
    "assign"  , Rules.BasicStmt.assign;
    "if"      , Rules.BasicStmt.if'; 
  ]

let test_space_expr_term expected init test_ctx = 
  let actual = ExprIntpr.space expr_rules init in
    assert_equal 1 (List.length actual)  ~printer:string_of_int;
    assert_equal expected (fst (List.hd actual)) ~printer:string_of_expr

let expr_term_1 = E.Const 1
let expr_term_2 = E.Binop ("+", E.Const 1, E.Const 4)
let expr_term_3 = E.Binop ("*", expr_term_2, expr_term_2)

let vfront = VF.create ()

let h = 
     H.create ()
  |> H.insert "x" 0 1 vfront
  |> H.insert "x" 1 2 vfront
  |> H.insert "y" 0 0 vfront

let curr_vf = 
     VF.create ()
  |> VF.update "x" 1
  |> VF.update "y" 0

let thrd = {T.curr = curr_vf}

let thrd_tree = T.Leaf thrd

let state = {ST.history = h; ST.threads = thrd_tree;}

let stmt_term_if_true = S.If (E.Const 1, S.AExpr (E.Const 1), S.AExpr (E.Const 0)) 
let stmt_term_if_false = S.If (E.Const 0, S.AExpr (E.Const 1), S.AExpr (E.Const 0)) 

let test_space_stmt_term expected init test_ctx = 
  let actual = StmtIntpr.space stmt_rules init in
    assert_equal 1 (List.length actual)  ~printer:string_of_int;
    assert_equal expected (fst (List.hd actual)) 
  

let print_hcell (l, t, v, _) = Printf.sprintf "(%s, %d, %d)" l t v

let suite =
  "suite">::: [
    "test_space_expr_term_1">:: test_space_expr_term (E.Const 1)  (expr_term_1, EC.default_state);
    "test_space_expr_term_2">:: test_space_expr_term (E.Const 5)  (expr_term_2, EC.default_state);
    "test_space_expr_term_3">:: test_space_expr_term (E.Const 25) (expr_term_3, EC.default_state);

    "test_space_expr_term_var">:: test_space_expr_term (E.Const 2) (E.Var "x", (h, thrd));

    "test_space_stmt_if_true">:: test_space_stmt_term (S.AExpr (E.Const 1)) (stmt_term_if_true, SC.default_state);
    "test_space_stmt_if_false">:: test_space_stmt_term (S.AExpr (E.Const 0)) (stmt_term_if_false, SC.default_state);

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
  (* ExprIntpr.space expr_rules (expr_term_2, EC.default_state) *)
