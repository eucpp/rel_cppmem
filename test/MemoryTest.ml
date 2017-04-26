open OUnit2
open MiniKanren
open Memory
open Lang
open Lang.Path
open TestUtils


module TS = ThreadState
module TT = Threads.Tree

let thrd_tree_tests =
  let vars       = ["r1"] in
  let atomics    = [] in
  let empty      = ThreadState.preallocate [] [] in
  let thrd       = ThreadState.preallocate vars atomics in
  let empty_node = TT.Node (empty, TT.Nil, TT.Nil) in
  let thrd_node  = TT.Node (thrd, TT.Nil, TT.Nil) in
  let tree1      = TT.Node (empty, thrd_node, empty_node) in
  let tree2      = TT.Node (empty, empty_node, thrd_node) in
  let tree3      = TT.Node (empty, thrd_node, thrd_node) in

  "thrd_tree">::: [
    "test_get_1">:: (fun test_ctx ->
      let stream   = run q (fun q -> Threads.geto (Threads.inj tree1) (pathl @@ pathn ()) q)
                            prj_stream
      in
      assert_single_answer thrd stream
    );

    "test_get_2">:: (fun test_ctx ->
      let stream   = run q (fun q -> Threads.geto (Threads.inj tree2) (pathr @@ pathn ()) q)
                            prj_stream
      in
      assert_single_answer thrd stream
    );

    "test_update_1">:: (fun test_ctx ->
      let stream   = run q (fun q -> Threads.seto (Threads.inj tree1) q (pathr @@ pathn ()) (TS.inj thrd))
                            prj_stream
      in
      assert_single_answer tree3 stream
    );

    "test_update_2">:: (fun test_ctx ->
      let stream   = run q (fun q -> Threads.seto (Threads.inj tree2) q (pathl @@ pathn ()) (TS.inj thrd))
                            prj_stream
      in
      assert_single_answer tree3 stream
    );

    "test_spawn">:: (fun test_ctx ->
      let expected = TT.Node (empty, empty_node, empty_node) in
      let stream   = run q (fun q -> Threads.spawno (Threads.inj empty_node) q @@ pathn ())
                            prj_stream
      in
      assert_single_answer expected stream
    );

    "test_join">:: (fun test_ctx ->
      let leaf a = TT.Node (a, TT.Nil, TT.Nil) in
      let parent_thrd = TS.create [("r1", 1)] [("x", 0); ("y", 0)] in
      let thrd1 = TS.create [("r1", 2)] [("x", 1); ("y", 0)] in
      let thrd2 = TS.create [("r1", 5)] [("x", 0); ("y", 1)] in
      let tree  = TT.Node (parent_thrd, leaf thrd1, leaf thrd2) in
      let expected = leaf @@ TS.create [("r1",1)] [("x", 1); ("y", 1)] in
      let stream   = run q (fun q -> Threads.joino (Threads.inj tree) q (pathn ()))
                            prj_stream
      in
      assert_single_answer expected stream
    )
  ]

let loc_story_tests =
"loc_story">::: [
  "test_read_acq_1" >:: (fun test_ctx ->
    let vf = ViewFront.from_list [("x", 0)] in
    let loc_story = LocStory.create 2 [(0, 0, vf); (1, 1, vf)] in
    let stream = run qrs (fun q  r  s  -> LocStory.read_acqo (LocStory.inj loc_story) (inj_nat 0) q r s)
                         (fun qs rs ss -> Utils.zip3 (prj_stream qs) (prj_stream rs) (prj_stream ss))
    in
    assert_stream [(Nat.of_int 0, Nat.of_int 0, vf); (Nat.of_int 1, Nat.of_int 1, vf)] stream
  );

  "test_read_acq_1" >:: (fun test_ctx ->
    let vf = ViewFront.from_list [("x", 0)] in
    let loc_story = LocStory.create 2 [(0, 0, vf); (1, 1, vf)] in
    let stream = run qrs (fun q  r  s  -> LocStory.read_acqo (LocStory.inj loc_story) (inj_nat 1) q r s)
                         (fun qs rs ss -> Utils.zip3 (prj_stream qs) (prj_stream rs) (prj_stream ss))
    in
    assert_stream [(Nat.of_int 1, Nat.of_int 1, vf)] stream
  );

  "test_write_rel" >:: (fun test_ctx ->
    let vf = ViewFront.from_list [("x", 0)] in
    let story = LocStory.create 0 [] in
    let expected = LocStory.create 1 [(0, 0, vf)] in
    let stream = run q (fun q -> LocStory.write_relo (LocStory.inj story) q (inj_nat 0) (ViewFront.inj vf))
                       prj_stream
    in
    assert_single_answer expected stream
  )
]

(*

let mem_state_tests =

  let vf        = ViewFront.from_assoc [("x", 0)] in
  let vf'       = ViewFront.from_assoc [("x", 1)] in
  let thrd      = { ThreadState.regs = Registers.empty; ThreadState.curr = vf; } in
  let thrd_tree = ThreadTree.Leaf thrd in

  "mem_state">::: [
    "test_read_acq">:: (fun test_ctx ->

      let mem_story = MemStory.from_assoc [("x", LocStory.from_list [(0, 0, vf); (1, 1, vf')])] in
      let mem_state = { MemState.thrds = thrd_tree; MemState.story = mem_story; MemState.scmem = SCMemory.empty} in

      let exp_thrd      = { ThreadState.regs = Registers.empty; ThreadState.curr = vf'; } in
      let exp_thrd_tree = ThreadTree.Leaf exp_thrd in
      let exp_mem_state = { MemState.thrds = exp_thrd_tree; MemState.story = mem_story; MemState.scmem = SCMemory.empty} in

      let stream = MemState.read_acq Path.N "x" mem_state in
      let show (v, state) = Printf.sprintf "Following value/state is not found among answers:\nx=%d;\nMemState:\n%s\n" v (MemState.show state) in
      let eq (v, state) (v', state') = (v == v') && (MemState.eq state state') in

        TestUtils.assert_stream stream [(0, mem_state); (1, exp_mem_state)] ~eq:eq ~show:show ~empty_check:false
    );

    "test_write_rel">:: (fun test_ctx ->

      let mem_story = MemStory.from_assoc [("x", LocStory.from_list [(0, 0, vf)]); ("y", LocStory.from_list [(0, 0, vf)])] in
      let mem_state = { MemState.thrds = thrd_tree; MemState.story = mem_story; MemState.scmem = SCMemory.empty} in

      let exp_thrd      = { ThreadState.regs = Registers.empty; ThreadState.curr = vf'; } in
      let exp_thrd_tree = ThreadTree.Leaf exp_thrd in
      let exp_mem_story = MemStory.from_assoc [("x", LocStory.from_list [(0, 0, vf); (1, 1, vf')]); ("y", LocStory.from_list [(0, 0, vf)])] in
      let exp_mem_state = { MemState.thrds = exp_thrd_tree; MemState.story = exp_mem_story; MemState.scmem = SCMemory.empty} in

        assert_equal exp_mem_state (MemState.write_rel Path.N "x" 1 mem_state) ~cmp:MemState.eq ~printer:MemState.show
    )

  ] *)

let tests =
  "memory">::: [
                thrd_tree_tests;
                loc_story_tests;
                (* loc_story_tests;
                mem_state_tests *)
  ]
