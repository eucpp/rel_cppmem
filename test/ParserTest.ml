open OUnit2

module ET = Lang.ExprTerm 
module EC = Lang.ExprContext
module ES = Memory.ThreadState

module ST = Lang.StmtTerm
module SC = Lang.StmtContext
module SS = Memory.MemState

let parse_expr str = 
  let lexbuf = Lexing.from_string str in
    Parser.expr_main Lexer.token lexbuf

(* module Tester *)
(*   (T : Lang.Term) =  *)
(*   struct *)
(*     let test_parse str expected test_ctx =  *)
(*       assert_equal expected () *)
(*   end *)

(* let str expected test_ctx =  *)
  

let expr_parser_tests =
  "expr">::: [
    "test_ret">:: fun test_ctx -> assert_equal (ET.Const 1) (parse_expr "1") ~cmp:ET.eq ~printer:ET.show; 
  ]

let tests = 
  "parser">::: [expr_parser_tests; ]
