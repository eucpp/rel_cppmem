open OUnit2
open MiniKanren
open MiniKanrenStd
open Memory
open Rules
open Lang
open TestUtils

module T = Lang.Term.T
module C = Lang.Context.T
module S = Memory.MemState

let test_step ?empty_check rules (t, s) expected test_ctx =
  let module Step = (val make_reduction_relation rules) in
  let module Sem = Semantics.Make(Step) in
  let t, s = Term.inj t, S.inj s in
  let stream = Sem.(
    run qr (fun q  r  -> (t, s) --> (Option.some @@ inj_pair q r))
           (fun qs rs -> Stream.zip (prj_stream qs) (prj_stream rs))
  ) in
  TestUtils.assert_stream expected stream ?empty_check ~printer:(fun (t,s) -> Term.pprint @@ Term.to_logic t)

let test_thrd_space ?empty_check rules path (t, s) expected test_ctx =
  let preconditiono c _ _ = Context.patho c (Path.inj path) in
  (* let reducibleo = fun (t, _) b -> Term.reducibleo ~path:(Path.inj path) t b in *)
  let module Step = (val make_reduction_relation ~ordero:(thrd_splito @@ Path.inj path) ~preconditiono rules) in
  let module Sem = Semantics.Make(Step) in

  let t, s = Term.inj t, S.inj s in
  let stream = Sem.(
    run qr (fun q  r  -> (t, s) -->* (q, r))
           (fun qs rs -> Stream.zip (prj_stream qs) (prj_stream rs))
  ) in
  (* let handler (t, s) =
    let t' = Term.pprint @@ Term.to_logic t in
    let s' = MemState.pprint @@ MemState.to_logic s in
    Printf.printf "\n%s\n%s\n" t' s'
  in
  let _ = Stream.iter handler stream in *)
  TestUtils.assert_stream ?empty_check expected stream

let test_space ?empty_check rules (t, s) expected test_ctx =
  let module Step = (val make_reduction_relation rules) in
  let module Sem = Semantics.Make(Step) in
  let t, s = Term.inj t, S.inj s in
  let stream = Sem.(
    run qr (fun q  r  -> (t, s) -->* (q, r))
           (fun qs rs -> Stream.zip (prj_stream qs) (prj_stream rs))
  ) in
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
      let na = ViewFront.from_list [("x", 0); ("y", 0)] in
      let sc = ViewFront.from_list [("x", 0); ("y", 0)] in
      let mem' = S.create (Threads.create [("r1", 42); ("r2", 0)] [("x", 0); ("y", 0)]) (MemStory.preallocate ["x"; "y"]) ~na ~sc in
      test_step [Basic.asgn] (T.Asgn (T.Var "r1", const 42), mem) [(T.Skip, mem')]
    );

    "if_true">:: test_step [Basic.if'] (T.If (const 1, T.Skip, T.Stuck), mem) [(T.Skip, mem)];

    "if_false">:: test_step [Basic.if'] (T.If (const 0, T.Stuck, T.Skip), mem) [(T.Skip, mem)];

    "if_expr">:: test_space Basic.all (T.If (T.Binop ("=", const 1, const 0), T.Stuck, T.Skip), mem) [(T.Skip, mem)];

    "repeat">:: (
      let loop = T.Repeat (const 1) in
      test_step [Basic.repeat] (loop, mem) [(T.If (const 1, T.Skip, loop), mem)]
    );

    "seq_skip">:: test_step Basic.all (T.Seq (T.Skip, T.Skip), mem) [(T.Skip, mem)];

    "thrd_space">:: (
      let thrd   = ThreadState.create [("r1",  0)] [] in
      let thrd'  = ThreadState.create [("r1", 42)] [] in
      let thrds  = Threads.(Tree.(Node (thrd, Node (thrd , Nil, Nil),  Node (thrd, Nil, Nil)))) in
      let thrds' = Threads.(Tree.(Node (thrd, Node (thrd', Nil, Nil),  Node (thrd, Nil, Nil)))) in
      let story  = MemStory.preallocate [] in
      let memx    = MemState.create thrds  story in
      let memx'   = MemState.create thrds' story in
      let thrd_code = T.Seq (T.Asgn (T.Var "r1", const 42), T.Var "r1") in
      (* let thrd_code = T.Skip in *)
      test_thrd_space Basic.all Path.(L N) (T.Par (thrd_code, T.Skip), memx) [(T.Par (const 42, T.Skip), memx')]
    )

    (* "seq_stuck">:: test_step Basic.all (T.Seq (T.Stuck, T.Skip), mem) [(T.Stuck, mem)]; *)

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

let thrd_spawning_tests =
  let mem  = S.preallocate ["r1"; "r2"] ["x"; "y"] in

  "thrd_spawning">::: [
    "spawn">:: (
      let thrd = ThreadState.preallocate ["r1"; "r2"] ["x"; "y"] in
      let leaf = Threads.Tree.Node (thrd, Threads.Tree.Nil, Threads.Tree.Nil) in
      let node = Threads.Tree.Node (thrd, leaf, leaf) in
      let mem' = S.create node (MemStory.preallocate ["x"; "y"])
        ~na:(ViewFront.from_list [("x", 0); ("y", 0)])
        ~sc:(ViewFront.from_list [("x", 0); ("y", 0)])
      in
      test_step [ThreadSpawning.spawn] (T.Spw (T.Skip, T.Skip), mem) [(T.Par (T.Skip, T.Skip), mem')]
    );

    "join">:: (
      let thrd = ThreadState.preallocate ["r1"; "r2"] ["x"; "y"] in
      let leaf = Threads.Tree.Node (thrd, Threads.Tree.Nil, Threads.Tree.Nil) in
      let node = Threads.Tree.Node (thrd, leaf, leaf) in
      let mem' = S.create node (MemStory.preallocate ["x"; "y"])
        ~na:(ViewFront.from_list [("x", 0); ("y", 0)])
        ~sc:(ViewFront.from_list [("x", 0); ("y", 0)])
      in
      test_step [ThreadSpawning.join] (T.Par (const 1, const 2), mem') [(T.Pair (const 1, const 2), mem)]
    );

    "spawn_assign">:: (
      let thrd = ThreadState.create [("r1", 42); ("r2", 1)] [("x", 0); ("y", 0)] in
      let node = Threads.Tree.Node (thrd, Threads.Tree.Nil, Threads.Tree.Nil) in
      let mem' = S.create node (MemStory.preallocate ["x"; "y"])
        ~na:(ViewFront.from_list [("x", 0); ("y", 0)])
        ~sc:(ViewFront.from_list [("x", 0); ("y", 0)])
      in
      let pair = T.Pair (T.Var "r1", T.Var "r2") in
      let stmt = T.Asgn (pair,T.Spw (const 42, const 1)) in
      test_space (ThreadSpawning.all @ Basic.all) (stmt, mem) [(T.Skip, mem')]
    );
  ]

let rel_acq_tests =

  let vf         = ViewFront.from_list [("x", 0)] in
  let vf'        = ViewFront.from_list [("x", 1)] in
  let thrd       = ThreadState.preallocate [] ["x"] in
  let thrd'      = ThreadState.create [] [("x", 1)] ~acq:[("x", 1)] ~rel:[("x", 0)] in
  let thrd''     = ThreadState.create [] [("x", 1)] ~acq:[("x", 1)] ~rel:[("x", 0)] in
  let tree       = Threads.Tree.Node (thrd,   Threads.Tree.Nil, Threads.Tree.Nil) in
  let tree'      = Threads.Tree.Node (thrd',  Threads.Tree.Nil, Threads.Tree.Nil) in
  let tree''     = Threads.Tree.Node (thrd'', Threads.Tree.Nil, Threads.Tree.Nil) in

  "rel_acq">::: [
    "read_acq">:: (
      let loc_story = LocStory.create 2 [(0, 0, vf); (1, 1, vf')] in
      let mem_story = MemStory.create [("x", loc_story)] in
      let na        = ViewFront.from_list [("x", 0)] in
      let state     = S.create tree  mem_story ~na in
      let state'    = S.create tree' mem_story ~na in
      test_step [RelAcq.read_acq] (T.Read (ACQ, "x"), state) [(const 0, state); (const 1, state')]
    );

    "write_rel">::(
      let loc_story  = LocStory.create 1 [(0, 0, vf)] in
      let loc_story' = LocStory.create 2 [(1, 1, vf); (0, 0, vf)] in
      let mem_story  = MemStory.create [("x", loc_story )] in
      let mem_story' = MemStory.create [("x", loc_story')] in
      let na         = ViewFront.from_list [("x", 0)] in
      let state      = S.create tree   mem_story  ~na in
      let state'     = S.create tree'' mem_story' ~na in
      test_step [RelAcq.write_rel] (T.Write (REL, "x", const 1), state) [(T.Skip, state')]);
  ]

let tests =
  "rules">::: [basic_tests; thrd_spawning_tests; rel_acq_tests]
