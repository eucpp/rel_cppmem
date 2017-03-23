open OUnit2
open MiniKanren
open Memory
open Rules
open Lang
open Semantics
open TestUtils

module T = Lang.Term
module C = Lang.Context
module S = Memory.MemState

let test_step ?empty_check rules (t, s) expected test_ctx =
  let sem    = make rules in
  let stream = run qr (fun q  r  -> stepo sem (inj_term t) (S.inj s) q r)
                      (fun qs rs -> Stream.zip (prj_stream qs) (prj_stream rs))
  in
  TestUtils.assert_stream ?empty_check expected stream

let test_space ?empty_check rules (t, s) expected test_ctx =
  let sem    = make rules in
  let stream = run qr (fun q  r  -> spaceo sem (inj_term t) (S.inj s) q r)
                      (fun qs rs -> Stream.zip (prj_stream qs) (prj_stream rs))
  in
  TestUtils.assert_stream ?empty_check expected stream

let const n = T.Const (Nat.of_int n)

let basic_tests =
  let mem  = S.preallocate ["r1"; "r2"] ["x"; "y"] in

  "basic">::: [
    "var">:: test_step [Basic.var] (T.Var "r1", mem) [(const 0, mem)];

    "binop">:: test_step [Basic.binop] (T.Binop ("+", const 1, const 2), mem) [(const 3, mem)];

    "binop_complex">:: (
      let mem' = S.create (Threads.create [("r1", 42)] []) (MemStory.preallocate []) in
      let e = (T.Binop ("+", T.Var "r1", T.Binop ("*", const 2, const 4)), mem') in
      test_space Basic.all e [(const 50, mem');]
    );

    "pair">:: test_space Basic.all (T.Pair (T.Var "r1", T.Var "r2"), mem) [(T.Pair (const 0, const 0), mem)];

    "assign">:: (
      let mem' = S.create (Threads.create [("r1", 42); ("r2", 0)] [("x", 0); ("y", 0)]) (MemStory.preallocate ["x"; "y"]) in
      test_step [Basic.asgn] (T.Asgn (T.Var "r1", const 42), mem) [(T.Skip, mem')]
    );

    "if_true">:: test_step [Basic.if'] (T.If (const 1, T.Skip, T.Stuck), mem) [(T.Skip, mem)];

    "if_false">:: test_step [Basic.if'] (T.If (const 0, T.Stuck, T.Skip), mem) [(T.Skip, mem)];

    "repeat">:: (
      let loop = T.Repeat (const 1) in
      test_step [Basic.repeat] (loop, mem) [(T.If (const 1, T.Skip, loop), mem)]
    );

    "seq_skip">:: test_step Basic.all (T.Seq (T.Skip, T.Skip), mem) [(T.Skip, mem)];

    "seq_stuck">:: test_step Basic.all (T.Seq (T.Stuck, T.Skip), mem) [(T.Stuck, mem)];

    "spawn">:: (
      let thrd = ThreadState.preallocate ["r1"; "r2"] ["x"; "y"] in
      let leaf = Threads.Tree.Node (thrd, Threads.Tree.Nil, Threads.Tree.Nil) in
      let node = Threads.Tree.Node (thrd, leaf, leaf) in
      let mem' = S.create node (MemStory.preallocate ["x"; "y"]) in
      test_step [Basic.spawn] (T.Spw (T.Skip, T.Skip), mem) [(T.Par (T.Skip, T.Skip), mem')]
    );

    "join">:: (
      let thrd = ThreadState.preallocate ["r1"; "r2"] ["x"; "y"] in
      let leaf = Threads.Tree.Node (thrd, Threads.Tree.Nil, Threads.Tree.Nil) in
      let node = Threads.Tree.Node (thrd, leaf, leaf) in
      let mem' = S.create node (MemStory.preallocate ["x"; "y"]) in
      test_step [Basic.join] (T.Par (const 1, const 2), mem') [(T.Pair (const 1, const 2), mem)]
    );

    "spawn_assign">:: (
      let thrd = ThreadState.create [("r1", 42); ("r2", 1)] [("x", 0); ("y", 0)] in
      let node = Threads.Tree.Node (thrd, Threads.Tree.Nil, Threads.Tree.Nil) in
      let mem' = S.create node (MemStory.preallocate ["x"; "y"]) in
      let pair = T.Pair (T.Var "r1", T.Var "r2") in
      let stmt = T.Asgn (pair,T.Spw (const 42, const 1)) in
      test_space Basic.all (stmt, mem) [(T.Skip, mem')]
    );

    (*
    "seq_asgn">:: (
      let stmt = T.Seq ((T.Asgn (T.Var "x", T.Const 42), T.Var "x")) in
      Tester.test_space Basic.all (stmt, S.empty) [(T.Const 42, mem)]
    );

    "space_all">:: (let pair = T.Pair (T.Var "x", T.Var "y") in
                    let stmt = T.Spw (
                      T.Seq (T.Asgn (T.Var "z", T.Const 42), T.Var "z"),
                      T.If (T.Const 1, T.Const 1, T.Const 0)
                   ) in
                      Tester.test_space ~empty_check:false Basic.all (stmt, S.empty) [(T.Pair (T.Const 42, T.Const 1), S.empty)]); *)

  ]

let rel_acq_tests =

  let vf         = ViewFront.from_list [("x", 0)] in
  let vf'        = ViewFront.from_list [("x", 1)] in
  let thrd       = ThreadState.create [] [("x", 0)] in
  let thrd'      = ThreadState.create [] [("x", 1)] in
  let tree       = Threads.Tree.Node (thrd,  Threads.Tree.Nil, Threads.Tree.Nil) in
  let tree'      = Threads.Tree.Node (thrd', Threads.Tree.Nil, Threads.Tree.Nil) in

  "rel_acq">::: [
    "read_acq">:: (
      let loc_story = LocStory.create 2 [(0, 0, vf); (1, 1, vf')] in
      let mem_story = MemStory.create [("x", loc_story)] in
      let state     = S.create tree mem_story in
      let state'    = S.create tree' mem_story in
      test_step [RelAcq.read_acq] (T.Read (ACQ, "x"), state) [(const 0, state); (const 1, state')]
    );

    "write_rel">::(
      let loc_story  = LocStory.create 1 [(0, 0, vf)] in
      let loc_story' = LocStory.create 2 [(1, 1, vf'); (0, 0, vf)] in
      let mem_story  = MemStory.create [("x", loc_story )] in
      let mem_story' = MemStory.create [("x", loc_story')] in
      let state      = S.create tree  mem_story in
      let state'     = S.create tree' mem_story' in
      test_step [RelAcq.write_rel] (T.Write (REL, "x", const 1), state) [(T.Skip, state')]);
  ]

let tests =
  "rules">::: [basic_tests; rel_acq_tests]
