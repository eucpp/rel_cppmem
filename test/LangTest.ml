open OUnit2
open MiniKanren

module ET = Lang.ExprTerm 
module EC = Lang.ExprContext
module ES = Lang.ExprState

module ST = Lang.StmtTerm
module SC = Lang.StmtContext
module SS = Lang.StmtState

module Tester
  (T : Lang.Term)
  (C : Lang.Context with type t = T.t with type lt' = T.lt')
  (S : Lang.State) =
  struct
    module Sem = Semantics.Make(T)(C)(S)

    let test_reducible pairs test_ctx =
      List.iter (fun (t, b) -> assert_equal b @@ Sem.reducible t) pairs  
      

    let test_split term expected test_ctx =
      let stream             = Sem.split term in
      let show (c, t)        = "Context/Term is not found among answers: " ^ C.show c ^ " ; " ^ T.show t in
      let eq (c, t) (c', t') = (C.eq c c') && (T.eq t t') in
        TestUtils.assert_stream stream expected ~show:show ~eq:eq

    let test_plug ctx_term expected test_ctx =
      let actual = Sem.plug ctx_term in
        assert_equal expected actual ~cmp:T.eq ~printer:T.show
  end

module ExprTester = Tester(ET)(EC)(ES)
module StmtTester = Tester(ST)(SC)(SS)

let expr_tests = 
  "expr">::: [
    "test_split_const"  >:: ExprTester.test_split (ET.Const 1) [(EC.Hole, ET.Const 1)];
    
    "test_split_var"    >:: ExprTester.test_split (ET.Var "x") [(EC.Hole, ET.Var "x")];
    
    "test_split_binop"  >:: (let e = ET.Binop ("+", ET.Var "x", ET.Const 42) 
                             in
                               ExprTester.test_split e [(EC.Hole, e);
                                                        (EC.BinopL ("+", EC.Hole, ET.Const 42), ET.Var "x");
                                                        (EC.BinopR ("+", ET.Var "x", EC.Hole), ET.Const 42);]);

    "test_plug_const"   >:: ExprTester.test_plug (EC.Hole, ET.Const 1) (ET.Const 1);

    "test_plug_var"     >:: ExprTester.test_plug (EC.Hole, ET.Var "x") (ET.Var "x");

    "test_plug_binop_1" >:: (let e = ET.Binop ("+", ET.Const 1, ET.Const 2) in
                               ExprTester.test_plug (EC.Hole, e) e);

    "test_plug_binop_3" >:: (let e = ET.Binop ("+", ET.Const 1, ET.Const 2) in
                               ExprTester.test_plug (EC.BinopL ("+", EC.Hole, ET.Const 2), ET.Const 1) e);

    "test_plug_binop_3" >:: (let e = ET.Binop ("+", ET.Var "x", ET.Const 2) in
                               ExprTester.test_plug (EC.BinopL ("+", EC.Hole, ET.Const 2), ET.Var "x") e);
    
    "test_plug_binop_4" >:: (let e = ET.Binop ("+", ET.Const 1, ET.Var "x") in
                               ExprTester.test_plug (EC.BinopR ("+", ET.Const 1, EC.Hole), ET.Var "x") e);
  ]

let stmt_tests = 
  "stmt_tests">::: [
    "test_reducible_pair">:: StmtTester.test_reducible 
                               [(ST.Pair (ET.Const 1, ET.Const 2), false);
                                (ST.Pair (ET.Const 1, ET.Var "x"), true);
                                (ST.Pair (ET.Var "x", ET.Const 2), true);
                                (ST.Pair (ET.Var "x", ET.Var "y"), true)];

    "test_split_expr">:: (let stmt = ST.AExpr (ET.Var "x") in
                            StmtTester.test_split stmt [(SC.Hole, stmt)]);

    "test_split_asgn">:: (let stmt = ST.Asgn (ST.AExpr (ET.Var "x"), ST.AExpr (ET.Const 1)) in
                              StmtTester.test_split stmt [(SC.Hole, stmt);
                                                          (SC.AsgnC (ST.AExpr (ET.Var "x"), SC.Hole), ST.AExpr (ET.Const 1))]);

    "test_plug_skip">:: StmtTester.test_plug (SC.Hole, ST.Skip) ST.Skip;
  ]

let tests = 
  "lang">::: [expr_tests; stmt_tests]

