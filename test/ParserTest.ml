open OUnit2

module ET = Lang.ExprTerm 
module EC = Lang.ExprContext
module ES = Lang.ExprState

module ST = Lang.StmtTerm
module SC = Lang.StmtContext
module SS = Lang.StmtState

let parse_expr str = 
  let lexbuf = Lexing.from_string str in
    Parser.main Lexer.token lexbuf

let expr_parser_tests =
  "expr">::: [
    "test_ret">:: fun test_ctx -> assert_equal (ET.Const 1) (parse_expr "1") ~cmp:ET.eq ~printer:ET.show; 
  ]

let tests = 
  "parser">::: [expr_parser_tests; ]
