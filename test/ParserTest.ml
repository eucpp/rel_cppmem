open OUnit2

module ET = Lang.ExprTerm 
module EC = Lang.ExprContext
module ES = Memory.ThreadState

module ST = Lang.StmtTerm
module SC = Lang.StmtContext
module SS = Memory.MemState

let parse str = 
  let lexbuf = Lexing.from_string str in
  Parser.main Lexer.token lexbuf

let test_parse str expected test_ctx =
  assert_equal expected (parse str) ~cmp:ST.eq ~printer:ST.show

let expr_parser_tests =
  "expr">::: [
    "test_const">:: test_parse "ret 1" (ST.AExpr (ET.Const 1));
    "test_var">:: test_parse "ret r1" (ST.AExpr (ET.Var "r1"));
    "test_binop">:: test_parse "ret 1+r1" (ST.AExpr (ET.Binop ("+", ET.Const 1, ET.Var "r1")));

    "test_asgn">:: test_parse "r1 := 0" (ST.Asgn (ST.AExpr (ET.Var "r1"), ST.AExpr (ET.Const 0)));
    "test_if">:: test_parse "if 1 then skip else stuck fi" (ST.If (ST.AExpr (ET.Const 1), ST.Skip, ST.Stuck));  

    "test_repeat">:: test_parse "repeat ret 1 end" (ST.Repeat (ST.AExpr (ET.Const 1)));

    "test_read">:: test_parse "x_acq" (ST.Read (Memory.ACQ, "x"));
    "test_write">:: test_parse "x_rel := 1" (ST.Write (Memory.REL, "x", ET.Const 1));

    "test_seq">:: test_parse "skip; stuck" (ST.Seq (ST.Skip, ST.Stuck));
  ]

let tests = 
  "parser">::: [expr_parser_tests; ]
