open OUnit2
open Memory

module T = Lang.Term

let test_pprint term s test_ctx = 
  assert_equal s (T.show term) ~printer:(fun x -> x)

let tests = 
  "pretty_print">::: [
    "test_binop">:: test_pprint (T.Binop ("+", T.Const 1, T.Const 2)) "1 + 2";
    "test_asgn">:: test_pprint (T.Asgn (T.Var "x", T.Const 1)) "x := 1";
    "test_if">:: test_pprint (T.If ((T.Const 1), T.Skip, T.Stuck)) "if 1\nthen skip\nelse stuck";
    "test_repeat">:: test_pprint (T.Repeat (T.Const 1)) "repeat 1";
    "test_read">:: test_pprint (T.Read (ACQ, "x")) "x_acq";
    "test_write">:: test_pprint (T.Write (REL, "x", T.Const 1)) "x_rel := 1";
    "test_seq">:: test_pprint (T.Seq (T.Skip, T.Stuck)) "skip;\nstuck";
    "test_spw">:: test_pprint (T.Spw (T.Skip, T.Stuck)) "spw {{{\n    skip\n|||\n    stuck\n}}}";

    "test_composite">:: test_pprint 
                          (T.Asgn (T.Pair (T.Var "x", T.Var "y"), 
                                  T.Spw (
                                      T.Seq (T.Asgn (T.Var "z", T.Const 1), T.Var "z"),
                                      T.Const 2
                                    )
                                 ))
                          (String.concat "\n" [
                          "(x, y) := spw {{{";
                          "              z := 1;";
                          "              z";
                          "          |||";
                          "              2";
                          "          }}}"])
  ]
