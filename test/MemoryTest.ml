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

    "test_join">:: (fun test_ctx ->
      let vf       = ViewFront.from_assoc [("x", 0); ("y", 1)] in
      let vf'      = ViewFront.from_assoc [("x", 1); ("z", 1)] in
      let expected = ViewFront.from_assoc [("x", 1); ("y", 1); ("z", 1)] in
      let actual   = ViewFront.join vf vf' in
        assert_equal expected actual ~cmp:ViewFront.eq ~printer:ViewFront.show
    );

        
    (* "test_join_tmp">:: (fun test_ctx -> *)
    (*   let vf       = ViewFront.from_assoc [("x", 0)] in *)
    (*   let vf'      = ViewFront.from_assoc [("x", 1)] in *)
    (*   let expected = ViewFront.from_assoc [("x", 1)] in *)
    (*   let actual   = ViewFront.join vf vf' in *)
    (*     assert_equal expected actual ~cmp:ViewFront.eq ~printer:ViewFront.show *)
    (* ); *)
  ]

(* let thrd_state_tests =  *)
(*   "thrd_state">::: [ *)
(*     "test_join_viewfront">:: (fun test_ctx -> *)
(*       let regs = Registers.empty in *)
(*       let curr = ViewFront.from_assoc [("x", 0)] in *)
(*       let vf   = ViewFront.from_assoc [("x", 1)] in *)
(*       let thrd = { ThreadState.regs = regs; ThreadState.curr = curr } in *)

(*       let expected = { ThreadState.regs = regs; ThreadState.curr = vf } in *)

(*       let actual = run q (fun q  -> ThreadState.join_viewfronto (ViewFront.inj vf) (ThreadState.inj thrd) q) *)
(*                          (fun qs -> ThreadState.prj @@ Utils.excl_answ qs) in *)
      
(*         assert_equal expected actual ~cmp:ThreadState.eq ~printer:ThreadState.show *)
(*     ) *)
(*   ] *)

let thrd_tree_tests = 

  let regs      = Registers.set "x" 0 Registers.empty in
  let thrd      = { ThreadState.regs = regs; ThreadState.curr = ViewFront.empty } in
  let thrd_tree = ThreadTree.Node (ThreadTree.Leaf thrd, ThreadTree.empty) in

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

  ]

let tests = 
  "memory">::: [registers_tests; 
                viewfront_tests;
                (* thrd_state_tests; *) 
                thrd_tree_tests; 
                loc_story_tests; 
                mem_state_tests]
