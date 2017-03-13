open OUnit2
open Lang

module T = Lang.Term
module C = Lang.Context
module S = Memory.MemState

let parse str =
  let lexbuf = Lexing.from_string str in
  Parser.main Lexer.token lexbuf

let test_parse str expected test_ctx =
  assert_equal expected (parse str) ~cmp:T.eq ~printer:T.show

let parser_tests =
  "expr">::: [
    "test_const">:: test_parse "ret 1" (T.Const 1);
    "test_var">:: test_parse "ret r1" (T.Var "r1");
    "test_binop">:: test_parse "ret 1+r1" (T.Binop ("+", T.Const 1, T.Var "r1"));

    "test_asgn">:: test_parse "r1 := 0" (T.Asgn (T.Var "r1", T.Const 0));
    "test_if">:: test_parse "if 1 then skip else stuck fi" (T.If (T.Const 1, T.Skip, T.Stuck));

    "test_repeat">:: test_parse "repeat ret 1 end" (T.Repeat (T.Const 1));

    "test_read">:: test_parse "x_acq" (T.Read (ACQ, "x"));
    "test_write">:: test_parse "x_rel := 1" (T.Write (REL, "x", T.Const 1));

    "test_seq">:: test_parse "skip; stuck" (T.Seq (T.Skip, T.Stuck));
    "test_spw">:: test_parse "spw {{{ skip ||| stuck }}}" (T.Spw (T.Skip, T.Stuck))
  ]

let tests =
  "parser">::: [parser_tests; ]
