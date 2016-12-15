open OUnit2
open MiniKanren
open Memory

let registers_tests = 
  "registers">::: [
    "test_1">:: (fun test_ctx -> 
       let r = Registers.set "x" 0 Registers.empty in
       let v = Registers.get "x" r in
         assert_equal 0 v ~printer:string_of_int   
    );

    "test_2">:: (fun test_ctx -> 
       let r  = Registers.set "x" 0 Registers.empty in
       let r' = Registers.set "x" 1 r in
       let v  = Registers.get "x" r' in
         assert_equal 1 v ~printer:string_of_int   
    );

    "test_3">:: (fun test_ctx ->
      let r  = Registers.set "x" 0 Registers.empty in
      let r' = Registers.set "y" 1 r in
      let vx = Registers.get "x" r' in
      let vy = Registers.get "y" r' in
        assert_equal 0 vx ~printer:string_of_int;
        assert_equal 1 vy ~printer:string_of_int   
    );
  ]

let viewfront_tests = 
  "viewfront">::: [
    "test_1">:: (fun test_ctx -> 
       let r = ViewFront.update "x" 0 ViewFront.empty in
       let v = ViewFront.get "x" r in
         assert_equal 0 v ~printer:string_of_int   
    );

    "test_2">:: (fun test_ctx -> 
       let r  = ViewFront.update "x" 0 ViewFront.empty in 
       let r' = ViewFront.update "x" 1 r in
       let v  = ViewFront.get "x" r' in
         assert_equal 1 v ~printer:string_of_int   
    );

    "test_3">:: (fun test_ctx ->
      let r  = ViewFront.update "x" 0 ViewFront.empty in
      let r' = ViewFront.update "y" 1 r in
      let vx = ViewFront.get "x" r' in
      let vy = ViewFront.get "y" r' in
        assert_equal 0 vx ~printer:string_of_int;
        assert_equal 1 vy ~printer:string_of_int   
    );

    (* "test_join">:: (fun test_ctx -> *)
    (*   let vf       = ViewFront.from_assoc [("x", 0); ("y", 1)] in *)
    (*   let vf'      = ViewFront.from_assoc [("x", 1); ("z", 1)] in *)
    (*   let expected = ViewFront.from_assoc [("x", 1); ("y", 1); ("z", 1)] in *)
    (*   let actual   = ViewFront.join vf vf' in *)
    (*     assert_equal expected actual ~cmp:ViewFront.eq ~printer:ViewFront.show *)
    (* ) *)
  ]

let regs      = Registers.set "x" 0 Registers.empty
let thrd      = { ThreadState.regs = regs; ThreadState.curr = ViewFront.empty }
let thrd_tree = ThreadTree.Node (ThreadTree.Leaf thrd, ThreadTree.empty)

let thrd_tree_tests = 
  "thrd_tree">::: [
    "test_get_1">:: (fun test_ctx -> 
      let expected = thrd in
      let actual = ThreadTree.get_thrd (Path.L Path.N) thrd_tree in 
        assert_equal expected actual ~cmp:ThreadState.eq ~printer:ThreadState.show 
    );

    "test_get_2">:: (fun test_ctx -> 
      let expected = ThreadState.empty in
      let actual   = ThreadTree.get_thrd (Path.R Path.N) thrd_tree in 
        assert_equal expected actual ~cmp:ThreadState.eq ~printer:ThreadState.show 
    );

    "test_update_1">:: (fun test_ctx ->
      let expected = thrd_tree in
      let actual   = ThreadTree.update_thrd (Path.L Path.N) thrd (ThreadTree.Node (ThreadTree.empty, ThreadTree.empty)) in
        assert_equal expected actual ~cmp:ThreadTree.eq ~printer:ThreadTree.show  
    );

    "test_update_2">:: (fun test_ctx ->
      let expected = ThreadTree.Node (ThreadTree.Leaf thrd, ThreadTree.Leaf thrd) in
      let actual   = ThreadTree.update_thrd (Path.R Path.N) thrd thrd_tree in
        assert_equal expected actual ~cmp:ThreadTree.eq ~printer:ThreadTree.show  
    );

    "test_spawn">:: (fun test_ctx -> 
      let leaf     = ThreadTree.Leaf ThreadState.empty in 
      let expected = ThreadTree.Node (ThreadTree.Node (leaf, leaf), leaf) in
      let actual   = ThreadTree.spawn_thrd (Path.L Path.N) (ThreadTree.Node (leaf, leaf)) in
        assert_equal expected actual ~cmp:ThreadTree.eq ~printer:ThreadTree.show  
    );

    "test_join">:: (fun test_ctx -> 
      let leaf     = ThreadTree.Leaf ThreadState.empty in 
      let expected = ThreadTree.Node (leaf, leaf) in
      let actual   = ThreadTree.join_thrd (Path.R Path.N) (ThreadTree.Node (leaf, ThreadTree.Node (leaf, leaf))) in
        assert_equal expected actual ~cmp:ThreadTree.eq ~printer:ThreadTree.show  
    )
  ]

let loc_story_tests = 
  "loc_story">::: [
    "test_read_acq_1">:: (fun test_ctx ->
      let vf = ViewFront.empty in
      let loc_story = LocStory.from_list [(0, 0, vf); (1, 0, vf)] in
        TestUtils.assert_stream (LocStory.read_acq loc_story 0) [(0, 0, vf); (1, 0, vf)] ~eq:Cell.eq ~show:Cell.show
    );

    "test_read_acq_2">:: (fun test_ctx ->
      let vf = ViewFront.empty in
      let loc_story = LocStory.from_list [(0, 0, vf); (1, 0, vf)] in
        TestUtils.assert_stream (LocStory.read_acq loc_story 1) [(1, 0, vf)] ~eq:Cell.eq ~show:Cell.show
    );

    "test_write_rel">:: (fun test_ctx ->
      let vf = ViewFront.empty in
      let expected = LocStory.from_list [(0, 0, vf)] in
        assert_equal (LocStory.write_rel 0 vf LocStory.empty) expected ~cmp:LocStory.eq ~printer:LocStory.show
    )
  ] 

let tests = 
  "memory">::: [registers_tests; viewfront_tests; thrd_tree_tests; loc_story_tests]
