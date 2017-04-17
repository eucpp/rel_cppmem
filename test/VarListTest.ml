open OUnit2
open MiniKanren
open VarList
open TestUtils

let rec to_mk_list = function
  | x::xs -> Cons (x, to_mk_list xs)
  | []    -> Nil

let inj_vars vars = inj_listi (List.map (fun (k, v) -> inj_pair !!k !!v) vars)

let inj_viewfront vf = inj_listi (List.map (fun (k, v) -> inj_pair !!k (inj_nat v)) vf)

let test_get vars var answ test_ctx =
  let stream = run q (fun q -> geto (inj_vars vars) !!var q) prj_stream in
  match answ with
    | Some value -> assert_single_answer ~printer:string_of_int value stream
    | None       -> assert_bool "Redundant answers in the stream (expected none)" (Stream.is_empty stream)

let test_set vars var value answ test_ctx =
  let stream = run q (fun q -> seto (inj_vars vars) q !!var !!value) prj_stream in
  match answ with
    | Some vars' -> assert_single_answer (to_mk_list vars') stream
    | None       -> assert_bool "Redundant answers in the stream (expected none)" (Stream.is_empty stream)

let test_join vars1 vars2 answ test_ctx =
  let stream  = run q (fun q  -> joino join_tso (inj_viewfront vars1) (inj_viewfront vars2) q)
                      (fun qs -> Stream.map (fun r -> List.to_list (fun (k, v) -> (k, Nat.to_int v)) r#prj) qs)
  in
  match answ with
    | Some vars' -> assert_single_answer vars' stream
    | None       -> assert_bool "Redundant answers in the stream (expected none)" (Stream.is_empty stream)

let tests =
  let vars = [("a", 1); ("b", 2); ("c", 3)] in
  "var-list">::: [
      "test_get1" >:: test_get vars "a" (Some 1);
      "test_get2" >:: test_get vars "b" (Some 2);
      "test_get3" >:: test_get vars "c" (Some 3);
      "test_get4" >:: test_get vars "d" None;

      "test_set1" >:: test_set vars "a" 0 (Some [("a", 0); ("b", 2); ("c", 3)]);
      "test_set2" >:: test_set vars "b" 0 (Some [("a", 1); ("b", 0); ("c", 3)]);
      "test_set3" >:: test_set vars "c" 0 (Some [("a", 1); ("b", 2); ("c", 0)]);
      "test_set4" >:: test_set vars "d" 0 None;

      "test_join1" >:: test_join [("x", 0); ("y", 1)] [("x", 1); ("y", 0)] (Some [("x", 1); ("y", 1)]);
      "test_join2" >:: test_join [("x", 0)] [("y", 0)] None;
  ]
