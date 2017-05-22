open OUnit2
open MiniKanren
open Lang
open Memory
open Memory.MemOrder
open TestUtils

open Term.T
open Context.T

let test_reducible pairs test_ctx =
  let reducible t = run q (fun q -> Context.reducibleo (Term.inj t) q) prj_stream in
  List.iter (fun (t, b) -> assert_single_answer b (reducible t)) pairs

let test_thrd_reducible triples test_ctx =
  let reducible t path = run q (fun q -> Context.thrd_reducibleo (Term.inj t) (Path.inj path) q) prj_stream in
  List.iter (fun (t, path, b) -> assert_single_answer b (reducible t path)) triples

let test_split term expected test_ctx =
  let split t = run qr (fun q  r  -> Context.splito (Term.inj t) q r)
                       (fun qs rs -> Stream.zip (prj_stream qs) (prj_stream rs))
  in
  let stream = split term in
  assert_single_answer expected stream

let test_plug ctx_term expected test_ctx =
  let plug (c, t) = run q (fun q -> Context.plugo q (Context.inj c) (Term.inj t)) prj_stream in
  let stream = plug ctx_term in
  assert_single_answer ~printer:(fun t -> Term.pprint @@ Term.to_logic t) expected stream

let test_pick_prm term expected test_ctx =
  let pick_prm t = run qrs (fun q  r  s  -> Context.pick_prmo (Term.inj t) q r s)
                           (fun qs rs ss -> Utils.zip3 (prj_stream qs) (prj_stream rs) (prj_stream ss))
  in
  let stream = pick_prm term in
  let printer (c, loc, n) = Printf.sprintf "%s:=%d" loc (Nat.to_int n) in
  assert_stream ~printer expected stream

let const n = Const (Nat.of_int n)

let tests =
  "lang">::: [
    "test_reducible_pair">:: test_reducible [
      (Pair (const 1, const 2), false);
      (Pair (const 1, Var "x"), true);
      (Pair (Var "x", const 2), true);
      (Pair (Var "x", Var "y"), true)
    ];

    "test_thrd_reducible">:: test_thrd_reducible [
      (Par (Skip, Skip), Path.(L N), false);
      (Par (Seq (Skip, Skip), Skip), Path.(L N), true);
    ];

    "test_split_const"  >:: test_split (const 1) (Hole, const 1);

    "test_split_var"    >:: test_split (Var "x") (Hole, Var "x");

    "test_split_binop"  >:: (let e = Binop ("+", Var "x", const 42)
                             in
                               test_split e (BinopL ("+", Hole, const 42), Var "x"));

    "test_split_repeat" >:: test_split (Repeat (const 1)) (Hole, Repeat (const 1));

    (* "test_split_seq"  >:: test_split (Seq (Skip, Skip)) (SeqC (Hole, Skip), Skip); *)

    (* "test_plug_seq"  >:: test_plug (SeqC (Hole, Skip), Skip) Skip; *)

    "test_split_asgn">:: (let stmt = Asgn (Var "x", const 1) in
                              test_split stmt (Hole, stmt));

    "test_plug_const"   >:: test_plug (Hole, const 1) (const 1);

    "test_plug_var"     >:: test_plug (Hole, Var "x") (Var "x");

    "test_plug_binop_1" >:: (let e = Binop ("+", const 1, const 2) in
                               test_plug (Hole, e) e);

    "test_plug_binop_2" >:: (let e = Binop ("+", const 1, const 2) in
                               test_plug (BinopL ("+", Hole, const 2), const 1) e);

    "test_plug_binop_3" >:: (let e = Binop ("+", Var "x", const 2) in
                               test_plug (BinopL ("+", Hole, const 2), Var "x") e);

    "test_plug_binop_4" >:: (let e = Binop ("+", const 1, Var "x") in
                               test_plug (BinopR ("+", const 1, Hole), Var "x") e);

    "test_plug_skip">:: test_plug (Hole, Skip) Skip;

    "test_preallocate">:: (fun test_ctx ->
                            let vars, atomics = Term.preallocate @@ Term.to_logic (Spw (Seq (Var "r1", Read (SC, "x")), Seq (Var "r1", Var "r2"))) in
                            assert_equal ["r2";"r1"] vars ~cmp:(=) ~printer:(String.concat ",");
                            assert_equal ["x"] atomics ~cmp:(=) ~printer:(String.concat ","));

    "test_pick_prm_1">:: (
      let t1 = Write (RLX, "x", const 1) in
      let t2 = Write (RLX, "y", const 1) in
      test_pick_prm (Seq (t1, t2)) [(SeqL (Hole, t2), "x", Nat.of_int 1); (SeqR (t1, Hole), "y", Nat.of_int 1)]
    );

    "test_pick_prm_2">:: (
      let t1 = Write (RLX, "x", const 1) in
      let t2 = Write (RLX, "y", const 1) in
      test_pick_prm (Par (t1, t2)) [(ParL (Hole, t2), "x", Nat.of_int 1); (ParR (t1, Hole), "y", Nat.of_int 1)]
    );

    "test_pick_prm_3">:: (
      let t1 = Write (RLX, "x", const 1) in
      let t2 = Write (RLX, "y", const 1) in
      let t3 = Write (RLX, "z", const 1) in
      test_pick_prm (Par (t1, Seq (t2, t3))) [
        (ParL (Hole, Seq (t2, t3)),  "x", Nat.of_int 1);
        (ParR (t1, SeqL (Hole, t3)), "y", Nat.of_int 1);
        (ParR (t1, SeqR (t2, Hole)), "z", Nat.of_int 1);
      ]
    );
  ]
