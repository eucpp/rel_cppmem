open OUnit2
open MiniKanren
open Lang
open Memory
open Semantics

let prj_stream stream = Stream.map (fun r -> r#prj) stream

let assert_single_answer ?cmp ?printer expected stream =
  let hd, tl = Stream.retrieve ~n:1 stream in
  assert_bool "Empty stream" (not (Stream.is_empty stream));
  assert_bool "More than one answer in the stream" (Stream.is_empty tl);
  assert_equal ?cmp ?printer expected (List.hd hd)

(** Calculates difference between two lists.
    Returns pair of lists.
    First list contains elements from [l1] that are not found in [l2].
    Second list contains elements from [l2] that are not found in [l1]. *)
let list_diff ?(cmp = (=)) l1 l2 =
  let not_in_lst l el = not (List.exists (cmp el) l) in
  let fst = List.filter (not_in_lst l2) l1 in
  let snd = List.filter (not_in_lst l1) l2 in
  (fst, snd)

let assert_contains show eq xs x =
  assert_bool (show x) @@ List.exists (eq x) xs

let assert_stream ?(empty_check = true)
                  ?(cmp = (=))
                  ?printer
                  expected stream
  =
  let len               = List.length expected in
  let (actual, stream') = Stream.retrieve ~n:len stream in
  let diff_plus, diff_minus = list_diff ~cmp expected actual in
  let diff_plus_msg = match printer with
    | Some p ->
      let answers = List.map p diff_plus in
      Printf.sprintf "Missing answers: %s" (String.concat "; " answers)
    | None -> "Missing answers"
  in
  let diff_minus_msg = match printer with
    | Some p ->
      let answers = List.map p diff_minus in
      Printf.sprintf "Redundant answers: %s" (String.concat "; " answers)
    | None -> "Redundant answers"
  in
  assert_bool diff_plus_msg  (diff_plus = []);
  assert_bool diff_minus_msg (diff_minus = []);
  if empty_check
   then
      assert_bool "More answers than expected" (Stream.is_empty stream')

(* module Sem = Semantics.Make(Lang.Term)(Lang.Context)(MemState) *)

let test_prog sem prog expected test_ctx =
  let show s  = "Outcome is not found among answers: " ^ s in
  let lexbuf  = Lexing.from_string prog in
  let term    = Parser.main Lexer.token lexbuf in
  let rs, vs  = preallocate term in
  let state   = MemState.preallocate rs vs in
  let stream  =
   run qr (fun q r ->
              (* fresh (s') *)
                spaceo sem (inj_term term) (MemState.inj state) q r)
                (* (List.appendo s' nil s)) *)
           (fun qs rs -> Stream.zip
             (prj_stream qs)
             (prj_stream rs))
  in
  let stream' = Stream.map (fun (t, s) -> Term.pprint t) stream in
  let cnt     = ref 0 in
  Stream.iter (
      fun (t, s) -> cnt := !cnt + 1;
      Printf.printf "\n%d: %s" !cnt (Term.pprint t)
      (* Printf.printf "\n%d: %s\n" !cnt (String.concat " -> " epath) *)
    ) stream;
  assert_stream ~empty_check:false expected stream' ~printer:show ~cmp:(=)
