open OUnit2
open MiniKanren

module ET = Lang.ExprTerm 
module EC = Lang.ExprContext

module Tester
  (T : Lang.Term)
  (C : Lang.Context with type t = T.t) =
  struct
    (* module R = Lang.Reducer(C) *)

    let test_split term expected test_ctx =
      let stream             = C.split term in
      let (actual, stream')  = Stream.retrieve ~n:(List.length expected) @@ stream in
      let show (c, t)        = "Context/Term is not found among answers: " ^ C.show c ^ " ; " ^ T.show t in
      let eq (c, t) (c', t') = (C.eq c c') && (T.eq t t') in
      let assert_contains x =
        assert_bool (show x) @@ List.exists (eq x) actual
      in
        assert_bool "More answers than expected" (Stream.is_empty stream');
        List.iter assert_contains expected

    let test_plug ctx_term expected test_ctx =
      let actual = C.plug ctx_term in
        assert_equal expected actual ~cmp:T.eq ~printer:T.show
  end

module ExprTester = Tester(ET)(EC)

(* let test_split term expected =  *)
(*       let stream             = C.split term in *)
(*       let (actual, stream')  = Stream.retrieve ~n:(List.length expected) @@ stream in *)
(*       let show (c, t)        = "Context/Term is not found among answers: " ^ EC.show c ^ " ; " ^ ET.show t in *)
(*       let eq (c, t) (c', t') = (C.eq c c') && (T.eq t t') in *)
(*       let assert_contains x =   *)
(*         assert_bool (show x) @@ List.exists (eq x) actual *)
(*       in *)
(*         assert_bool "More answers than expected" (Stream.is_empty stream'); *)
(*         List.iter assert_contains expected *)

(*     let test_plug ctx_term expected =  *)
(*       let actual = C.plug ctx_term in *)
(*         assert_equal expected actual ~cmp:T.eq ~printer:T.show          *)

let expr_tests = 
  "expr">::: [
    "test_split_const"  >:: ExprTester.test_split (ET.Const 1) [(EC.Hole, ET.Const 1)];
    
    "test_split_var"    >:: ExprTester.test_split (ET.Var "x") [(EC.Hole, ET.Var "x")];
    
    "test_split_binop_1">:: (let e = ET.Binop ("+", ET.Const 1, ET.Const 2) in
                               ExprTester.test_split e [(EC.Hole, e)]);
    
    "test_split_binop_2">:: (let e = ET.Binop ("+", ET.Var "x", ET.Const 2) in
                               ExprTester.test_split e [(EC.BinopL ("+", EC.Hole, ET.Const 2), ET.Var "x")]);
    
    "test_split_binop_3">:: (let e = ET.Binop ("+", ET.Const 1, ET.Var "x") in
                               ExprTester.test_split e [(EC.BinopR ("+", ET.Const 1, EC.Hole), ET.Var "x")]);
    
    "test_split_binop_4">:: (let e = ET.Binop ("+", ET.Var "x", ET.Var "y") in
                               ExprTester.test_split e [(EC.BinopL ("+", EC.Hole, ET.Var "y"), ET.Var "x");
                                                        (EC.BinopR ("+", ET.Var "x", EC.Hole), ET.Var "y")]);

    (* "test_plug_const"   >:: test_expr_plug (EC.Hole, ET.Const 1) [ET.Const 1]; *)

    (* "test_plug_var"     >:: test_expr_plug (EC.Hole, ET.Var "x") [ET.Var "x"]; *)

    (* "test_plug_binop_1" >:: (let e = ET.Binop ("+", ET.Const 1, ET.Const 2) in *)
    (*                            test_expr_plug (EC.Hole, e) [e]); *)

    (* "test_plug_binop_2" >:: (let e = ET.Binop ("+", ET.Var "x", ET.Const 2) in *)
    (*                            test_expr_plug (EC.BinopL ("+", EC.Hole, ET.Const 2), ET.Var "x") [e]); *)
    
    (* "test_plug_binop_3" >:: (let e = ET.Binop ("+", ET.Const 1, ET.Var "x") in *)
    (*                            test_expr_plug (EC.BinopR ("+", ET.Const 1, EC.Hole), ET.Var "x") [e]); *)
  ]

let _ =
  run_test_tt_main expr_tests

