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

let assert_lists ?(cmp = (=))
                 ?printer
                 expected actual
   =
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
   assert_bool diff_minus_msg (diff_minus = [])

let assert_stream ?(empty_check = true)
                  ?(cmp = (=))
                  ?printer
                  expected stream
  =
  let len               = List.length expected in
  let (actual, stream') = Stream.retrieve ~n:len stream in
  assert_lists expected actual ~cmp ?printer;
  if empty_check
   then
      assert_bool "More answers than expected" (Stream.is_empty stream')

(* module Sem = Semantics.Make(Lang.Term)(Lang.Context)(MemState) *)

let test_prog ?n sem prog expected test_ctx =
  let show s  = "Outcome is not found among answers: " ^ s in
  let lexbuf  = Lexing.from_string prog in
  let term    = Parser.main Lexer.token lexbuf in
  let rs, vs  = preallocate term in
  let state   = MemState.preallocate rs vs in
  let stream  =
   run qr (fun q  r  -> spaceo sem (inj_term term) (MemState.inj state) q r)
          (fun qs rs ->
            (* let file = open_out "subst.log" in
            (try
              let fmt = Format.formatter_of_out_channel file in
              Stream.iter (fun r -> r#log fmt) qs;
            with
              | _ -> close_out file); *)
            Stream.iter (fun r -> Bdd.print_to_dot (r#encode ()) ~file:"subst.dot") qs;
            Stream.zip (prj_stream qs) (prj_stream rs))
  in
  let module S = Set.Make(String) in
  let set = ref S.empty in
  let cnt = ref 0 in
  let handler (t, s) =
    let answer = Term.pprint t in
    let set'   = S.add answer !set in
    cnt := !cnt + 1;
    set := set';
    Printf.printf "\n%d: %s" !cnt answer
  in
  let _ = match n with
    | Some n -> List.iter handler @@ fst @@ Stream.retrieve ~n:n stream
    | None   -> Stream.iter handler stream
  in
  assert_lists expected (S.elements !set) ~printer:show ~cmp:(=)
