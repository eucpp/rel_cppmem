open OUnit2
open MiniKanren

module LangTester
  (T : Lang.ATerm)
  (C : Lang.AContext with type t = T.t with type lt' = T.lt')
  (S : Lang.AState) =
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

module Tester = LangTester(Lang.Term)(Lang.Context)(Memory.MemState)

module T = Lang.Term
module C = Lang.Context

let tests =
  "lang">::: [
    "test_reducible_pair">:: Tester.test_reducible
                               [(T.Pair (T.Const 1, T.Const 2), false);
                                (T.Pair (T.Const 1, T.Var "x"), true);
                                (T.Pair (T.Var "x", T.Const 2), true);
                                (T.Pair (T.Var "x", T.Var "y"), true)];

    "test_split_const"  >:: Tester.test_split (T.Const 1) [(C.Hole, T.Const 1)];

    "test_split_var"    >:: Tester.test_split (T.Var "x") [(C.Hole, T.Var "x")];

    "test_split_binop"  >:: (let e = T.Binop ("+", T.Var "x", T.Const 42)
                             in
                               Tester.test_split e [(C.BinopL ("+", C.Hole, T.Const 42), T.Var "x");]);

    "test_split_seq"  >:: Tester.test_split (T.Seq (Skip, Skip)) [(C.SeqC (C.Hole, T.Skip), T.Skip)];

    "test_plug_seq"  >:: Tester.test_plug (C.SeqC (C.Hole, T.Skip), T.Skip) T.Skip;

    "test_plug_const"   >:: Tester.test_plug (C.Hole, T.Const 1) (T.Const 1);

    "test_plug_var"     >:: Tester.test_plug (C.Hole, T.Var "x") (T.Var "x");

    "test_plug_binop_1" >:: (let e = T.Binop ("+", T.Const 1, T.Const 2) in
                               Tester.test_plug (C.Hole, e) e);

    "test_plug_binop_2" >:: (let e = T.Binop ("+", T.Const 1, T.Const 2) in
                               Tester.test_plug (C.BinopL ("+", C.Hole, T.Const 2), T.Const 1) e);

    "test_plug_binop_3" >:: (let e = T.Binop ("+", T.Var "x", T.Const 2) in
                               Tester.test_plug (C.BinopL ("+", C.Hole, T.Const 2), T.Var "x") e);

    "test_plug_binop_4" >:: (let e = T.Binop ("+", T.Const 1, T.Var "x") in
                               Tester.test_plug (C.BinopR ("+", T.Const 1, C.Hole), T.Var "x") e);

    "test_split_asgn">:: (let stmt = T.Asgn (T.Var "x", T.Const 1) in
                              Tester.test_split stmt [(C.Hole, stmt);
                                                          (C.AsgnC (T.Var "x", C.Hole), T.Const 1)]);

    "test_plug_skip">:: Tester.test_plug (C.Hole, T.Skip) T.Skip;

    "test_preallocate">:: (fun test_ctx ->
                            let vars, atomics = Lang.Term.preallocate (T.Spw (T.Seq (T.Var "r1", T.Read (Lang.SC, "x")), T.Seq(T.Var "r1", T.Var "r2"))) in
                            assert_equal ["r2";"r1"] vars ~cmp:(=) ~printer:(String.concat ",");
                            assert_equal ["x"] atomics ~cmp:(=) ~printer:(String.concat ","))
  ]
