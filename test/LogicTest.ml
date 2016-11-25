open OUnit2
open MiniKanren

module ET = Logic.ExprTerm 
module EC = Logic.ExprContext

let test_stream_answers stream expected eq show test_ctxt =
  let (actual, stream') = Stream.retrieve ~n:(List.length expected) @@ stream in
  let assert_contains x =  
    assert_bool (show x) @@ List.exists (eq x) actual
  in
    assert_bool "More answers than expected" (Stream.is_empty stream');
    List.map assert_contains expected;
    ()

let test_expr_split expr expected = 
  let show (c, t)        = "Context/Term is not found among answers: " ^ EC.show c ^ " ; " ^ ET.show t in
  let eq (c, t) (c', t') = (EC.eq c c') && (ET.eq t t') in
    test_stream_answers (EC.split expr) expected eq show 
    
let test_expr_plug ctx_term expected =
  test_stream_answers (EC.plug ctx_term) expected ET.eq ET.show


let tests = 
  "expr">::: [
    "test_split_const"  >:: test_expr_split (ET.Const 1) [(EC.Hole, ET.Const 1)];
    
    "test_split_var"    >:: test_expr_split (ET.Var "x") [(EC.Hole, ET.Var "x")];
    
    "test_split_binop_1">:: (let e = ET.Binop ("+", ET.Const 1, ET.Const 2) in
                               test_expr_split e [(EC.Hole, e)]);
    
    "test_split_binop_2">:: (let e = ET.Binop ("+", ET.Var "x", ET.Const 2) in
                               test_expr_split e [(EC.BinopL ("+", EC.Hole, ET.Const 2), ET.Var "x")]);
    
    "test_split_binop_3">:: (let e = ET.Binop ("+", ET.Const 1, ET.Var "x") in
                               test_expr_split e [(EC.BinopR ("+", ET.Const 1, EC.Hole), ET.Var "x")]);
    
    "test_split_binop_4">:: (let e = ET.Binop ("+", ET.Var "x", ET.Var "y") in
                               test_expr_split e [(EC.BinopL ("+", EC.Hole, ET.Var "y"), ET.Var "x");
                                                  (EC.BinopR ("+", ET.Var "x", EC.Hole), ET.Var "y")]);

    "test_plug_const"   >:: test_expr_plug (EC.Hole, ET.Const 1) [ET.Const 1];

    "test_plug_var"     >:: test_expr_plug (EC.Hole, ET.Var "x") [ET.Var "x"];

    "test_plug_binop_1" >:: (let e = ET.Binop ("+", ET.Const 1, ET.Const 2) in
                               test_expr_plug (EC.Hole, e) [e]);

    "test_plug_binop_2" >:: (let e = ET.Binop ("+", ET.Var "x", ET.Const 2) in
                               test_expr_plug (EC.BinopL ("+", EC.Hole, ET.Const 2), ET.Var "x") [e]);
    
    "test_plug_binop_3" >:: (let e = ET.Binop ("+", ET.Const 1, ET.Var "x") in
                               test_expr_plug (EC.BinopR ("+", ET.Const 1, EC.Hole), ET.Var "x") [e]);
  ]

let _ =
  run_test_tt_main tests

