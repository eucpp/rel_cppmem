open OUnit2
open MiniKanren
open MiniKanrenStd
open MiniKanrenUtils
open Lang
open Memory
open Memory.MemOrder
open TestUtils

open Term.T
open Context.T

let string_of_bool = function
  | true -> "true"
  | false -> "false"

let logger = TreeLogger.create ()

let formatter = Format.formatter_of_out_channel @@ open_out "tmp.txt"

let test_order order pairs test_ctx =
  let stream t = run q
    (fun q ->
      fresh (dec x)
        (order (Term.inj t) dec)
        (conde [
          (dec === Option.none ()) &&& (q === !!false);
          (dec === Option.some x)  &&& (q === !!true);
        ])
    ) prj_stream in
  List.iter (fun (t, b) -> assert_single_answer ~printer:string_of_bool b (stream t)) pairs

let test_reducible = test_order splito

(* let test_can_prm = test_order promiseo *)

let test_thrd_reducible triples test_ctx =
  let reducible t path =
    run ~listener:(logger :> Listener.t) q
      (fun q ->
        fresh (dec x)
          (thrd_splito (Path.inj path) (Term.inj t) dec)
          (conde [
            (dec === Option.none ()) &&& (q === !!false);
            (dec === Option.some x)  &&& (q === !!true);
          ])
      ) prj_stream in
  List.iter (fun (t, path, b) -> assert_single_answer b (reducible t path); logger#print formatter) triples

let test_split term expected test_ctx =
  let split t = run qr
    (fun q r ->
      fresh (dec )
        (splito (Term.inj t) dec)
        (Decay.redexo dec r)
    )
    (fun qs rs -> (*Stream.zip (prj_stream qs)*) (prj_stream rs))
  in
  let stream = split term in
  assert_single_answer expected stream

(* let test_plug ctx_term expected test_ctx =
  let plug (c, t) = run q (fun q -> plugo (Context.inj c) (Term.inj t) q) prj_stream in
  let stream = plug ctx_term in
  assert_single_answer ~printer:(fun t -> Term.pprint @@ Term.to_logic t) expected stream *)

let test_pick_prm term expected test_ctx =
  let pick_prm t = run qr
    (fun q r ->
      fresh (dec)
        (promiseo (Term.inj t) dec)
        (Decay.redexo dec r)
    )
    (fun qs rs -> (*Stream.zip (prj_stream qs) (prj_stream rs)*) prj_stream rs)
  in
  let stream = pick_prm term in
  let printer t = Printf.sprintf "%s" @@ Term.pprint (Term.to_logic t) in
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

    (* "test_thrd_reducible">:: test_thrd_reducible [
      (Par (Skip, Skip), Path.(L N), false);
      (Par (Seq (Skip, Skip), Skip), Path.(L N), true);
    ]; *)

    (* "test_split_const"  >:: test_split (const 1) (const 1) (*(Hole, const 1)*); *)

    "test_split_var"    >:: test_split (Var "x") (Var "x") (*(Hole, Var "x")*);

    "test_split_binop"  >:: (let e = Binop ("+", Var "x", const 42)
                             in
                               test_split e (Var "x")) (*(BinopL ("+", Hole, const 42), Var "x"))*);

    "test_split_repeat" >:: test_split (Repeat (const 1)) (Repeat (const 1)) (*(Hole, Repeat (const 1))*);

    (* "test_split_seq"  >:: test_split (Seq (Skip, Skip)) (SeqC (Hole, Skip), Skip); *)

    (* "test_plug_seq"  >:: test_plug (SeqC (Hole, Skip), Skip) Skip; *)

    "test_split_asgn">:: (let stmt = Asgn (Var "x", const 1) in
                              test_split stmt stmt (*(Hole, stmt))*) );

    (* "test_plug_const"   >:: test_plug (Hole, const 1) (const 1);

    "test_plug_var"     >:: test_plug (Hole, Var "x") (Var "x");

    "test_plug_binop_1" >:: (let e = Binop ("+", const 1, const 2) in
                               test_plug (Hole, e) e);

    "test_plug_binop_2" >:: (let e = Binop ("+", const 1, const 2) in
                               test_plug (BinopL ("+", Hole, const 2), const 1) e);

    "test_plug_binop_3" >:: (let e = Binop ("+", Var "x", const 2) in
                               test_plug (BinopL ("+", Hole, const 2), Var "x") e);

    "test_plug_binop_4" >:: (let e = Binop ("+", const 1, Var "x") in
                               test_plug (BinopR ("+", const 1, Hole), Var "x") e);

    "test_plug_skip">:: test_plug (Hole, Skip) Skip; *)

    (* "test_preallocate">:: (fun test_ctx ->
                            let vars, atomics = Term.preallocate @@ Term.to_logic (Spw (Seq (Var "r1", Read (SC, "x")), Seq (Var "r1", Var "r2"))) in
                            assert_equal ["r2";"r1"] vars ~cmp:(=) ~printer:(String.concat ",");
                            assert_equal ["x"] atomics ~cmp:(=) ~printer:(String.concat ",")); *)

    (* "test_can_prm">:: (
      let t1 = Write (RLX, "x", const 1) in
      let t2 = Write (RLX, "y", const 1) in
      test_can_prm [
        (Seq (t1, t2), true);
        (Seq (Skip, t2), true);
        (Seq (t1, Skip), true);
        (Seq (Skip, Skip), false);

        (Par (t1, t2), true);
        (Par (Skip, t2), true);
        (Par (t1, Skip), true);
        (Par (Skip, Skip), false);
      ];
    ); *)

    "test_pick_prm_1">:: (
      let t1 = Write (RLX, "x", const 1) in
      let t2 = Write (RLX, "y", const 1) in
      test_pick_prm (Seq (t1, t2)) [t1; t2] (*[(SeqL (Hole, t2), t1); (SeqR (t1, Hole), t2)]*)
    );

    "test_pick_prm_2">:: (
      let t1 = Write (RLX, "x", const 1) in
      let t2 = Write (RLX, "y", const 1) in
      test_pick_prm (Par (t1, t2)) [t1; t2] (*[(ParL (Hole, t2), t1); (ParR (t1, Hole), t2)]*)
    );

    "test_pick_prm_3">:: (
      let t1 = Write (RLX, "x", const 1) in
      let t2 = Write (RLX, "y", const 1) in
      let t3 = Write (RLX, "z", const 1) in
      test_pick_prm (Par (t1, Seq (t2, t3))) [
        t1; t2; t3
        (* (ParL (Hole, Seq (t2, t3)),  t1);
        (ParR (t1, SeqL (Hole, t3)), t2);
        (ParR (t1, SeqR (t2, Hole)), t3); *)
      ]
    );
  ]
