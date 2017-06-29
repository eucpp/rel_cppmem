open MiniKanren
open MiniKanrenStd
open Lang
open Memory.MemOrder
open OUnit2
open Memory

module T = Lang.Term.T

let test_pprint term s test_ctx =
  assert_equal s (Term.pprint @@ Term.to_logic term) ~printer:(fun x -> x)

let const n = T.Const (Nat.of_int n)


let lang_tests =
  "lang">::: [
    "test_binop">:: test_pprint (T.Binop ("+", const 1, const 2)) "1 + 2";
    "test_asgn">:: test_pprint (T.Asgn (T.Var "x", const 1)) "x := 1";
    "test_if">:: test_pprint (T.If ((const 1), T.Skip, T.Stuck)) "if 1\nthen skip\nelse stuck";
    "test_repeat">:: test_pprint (T.Repeat (const 1)) "repeat 1 end";
    "test_read">:: test_pprint (T.Read (ACQ, "x")) "x_acq";
    "test_write">:: test_pprint (T.Write (REL, "x", const 1)) "x_rel := 1";
    "test_seq">:: test_pprint (T.Seq (T.Skip, T.Stuck)) "skip;\nstuck";
    "test_spw">:: test_pprint (T.Spw (T.Skip, T.Stuck)) "spw {{{\n    skip\n|||\n    stuck\n}}}";

    "test_composite">:: test_pprint
                          (T.Asgn (T.Pair (T.Var "x", T.Var "y"),
                                  T.Spw (
                                      T.Seq (T.Asgn (T.Var "z", const 1), T.Var "z"),
                                      const 2
                                    )
                                 ))
                          (String.concat "\n" [
                          "(x, y) := spw {{{";
                          "              z := 1;";
                          "              z";
                          "          |||";
                          "              2";
                          "          }}}"])
  ]

let mem_tests =
  "memory">::: [
    "test_thrd">:: (fun test_ctx ->
      let thrd = ThreadState.create [("r1", 0); ("r2", 42)] [("x", 1); ("y", 0)] ~acq:[("x", 1); ("y", 1)] ~rel:[("x", 0); ("y", 0)] in
      let str = ThreadState.pprint @@ ThreadState.to_logic thrd in
      Printf.printf "\nTEST_THRD:\n%s\n" str
    );

    "test_thrds">:: Threads.(Tree.(fun test_ctx ->
      let thrd1 = ThreadState.to_logic @@ ThreadState.preallocate ["r1"] ["x"] in
      let thrd2 = ThreadState.to_logic @@ ThreadState.preallocate ["r1"] ["x"] in
      let thrds = Value (Node (thrd1, Var (0, []), Value (Node (thrd2, Value Nil, Value Nil)))) in
      Printf.printf "\nTEST_THRDS:\n%s\n" (Threads.pprint thrds)
    ));

    "test_mem_story">:: (fun test_ctx ->
      let story = MemStory.to_logic @@ MemStory.preallocate ["x"; "y"] in
      Printf.printf "\nTEST_MEMSTORY:\n%s\n" (MemStory.pprint story)
    );

    "test_memstate">:: Threads.(Tree.(fun test_ctx ->
      let thrd  = ThreadState.preallocate ["r"] ["x"] in
      let thrds = Node (thrd, Nil, Nil) in
      let story = MemStory.preallocate ["x"] in
      let state = MemState.to_logic @@ MemState.create thrds story in
      Printf.printf "\nTEST_MEMSTATE:\n%s\n" (MemState.pprint state)
    ))
  ]

let tests =
  "pretty_print">::: [lang_tests; mem_tests]
