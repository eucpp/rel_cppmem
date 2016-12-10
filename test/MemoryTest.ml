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
       let r   = ViewFront.update "x" 0 ViewFront.empty in 
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
    )
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
  ]

let tests = 
  "memory">::: [registers_tests; viewfront_tests; thrd_tree_tests]
