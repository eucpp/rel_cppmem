open OUnit2;;
open Interpreter;;
  
let test_ctx_path = Seq (ParL (Hole, Const 0), Const 42);;

let test_getPath test_ctxt = assert_equal (L N) (getPath test_ctx_path);;

let suite =
  "interpreter">:::
    ["test getPath">:: test_getPath]
;;

let _ =
  run_test_tt_main suite
;;                       
