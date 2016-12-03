open OUnit2
open MiniKanren
open Memory

let int_of_bool b = if b then 1 else 0

let registers_test = 
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

    (* "test_tmp">:: (fun test_ctx -> *)
    (*   run q (fun q  -> Registers.key_eq !!"x" !!(!!"x", !!1) q) *)
    (*         (fun qs -> assert (not (Stream.is_empty qs)); *)
    (*                    Stream.iter (fun x -> print_int @@ int_of_bool (!? x)) qs) *)
    (* ); *)
  ]

let tests = 
  "memory">::: [registers_test]
