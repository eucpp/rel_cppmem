open OUnit2
open MiniKanren
open Lang
open TestUtils

open Term
open Context

let test_reducible pairs test_ctx =
  let reducible t = run q (fun q  -> reducibleo (inj_term t) q) prj_stream in
  List.iter (fun (t, b) -> assert_single_answer b (reducible t)) pairs

let test_split term expected test_ctx =
  let split t = run qr (fun q  r  -> splito (inj_term t) q r)
                       (fun qs rs -> Stream.zip (prj_stream qs) (prj_stream rs))
  in
  let stream = split term in
  assert_single_answer expected stream

let test_plug ctx_term expected test_ctx =
  let plug (c, t) = run q (fun q  -> plugo q (inj_context c) (inj_term t)) prj_stream in
  let stream = plug ctx_term in
  assert_single_answer ~printer:Term.pprint expected stream

let tests =
  "lang">::: [
    "test_reducible_pair">:: test_reducible
                               [(Pair (Const 1, Const 2), false);
                                (Pair (Const 1, Var "x"), true);
                                (Pair (Var "x", Const 2), true);
                                (Pair (Var "x", Var "y"), true)];

    "test_split_const"  >:: test_split (Const 1) (Hole, Const 1);

    "test_split_var"    >:: test_split (Var "x") (Hole, Var "x");

    "test_split_binop"  >:: (let e = Binop ("+", Var "x", Const 42)
                             in
                               test_split e (BinopL ("+", Hole, Const 42), Var "x"));

    "test_split_seq"  >:: test_split (Seq (Skip, Skip)) (SeqC (Hole, Skip), Skip);

    "test_plug_seq"  >:: test_plug (SeqC (Hole, Skip), Skip) Skip;

    "test_plug_const"   >:: test_plug (Hole, Const 1) (Const 1);

    "test_plug_var"     >:: test_plug (Hole, Var "x") (Var "x");

    "test_plug_binop_1" >:: (let e = Binop ("+", Const 1, Const 2) in
                               test_plug (Hole, e) e);

    "test_plug_binop_2" >:: (let e = Binop ("+", Const 1, Const 2) in
                               test_plug (BinopL ("+", Hole, Const 2), Const 1) e);

    "test_plug_binop_3" >:: (let e = Binop ("+", Var "x", Const 2) in
                               test_plug (BinopL ("+", Hole, Const 2), Var "x") e);

    "test_plug_binop_4" >:: (let e = Binop ("+", Const 1, Var "x") in
                               test_plug (BinopR ("+", Const 1, Hole), Var "x") e);

    "test_split_asgn">:: (let stmt = Asgn (Var "x", Const 1) in
                              test_split stmt (Hole, stmt));

    "test_plug_skip">:: test_plug (Hole, Skip) Skip;

    "test_preallocate">:: (fun test_ctx ->
                            let vars, atomics = preallocate (Spw (Seq (Var "r1", Read (Lang.SC, "x")), Seq (Var "r1", Var "r2"))) in
                            assert_equal ["r2";"r1"] vars ~cmp:(=) ~printer:(String.concat ",");
                            assert_equal ["x"] atomics ~cmp:(=) ~printer:(String.concat ","))
  ]
