open OUnit2
open MiniKanren
open Lang
open Memory

(* module T = Lang.Term.T *)

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

let test_prog ?n prog expected test_ctx =
  let module Sem = Semantics.Make(Semantics.OperationalStep) in
  let show s  = "Outcome is not found among answers: " ^ s in
  let lexbuf  = Lexing.from_string prog in
  let term    = Term.from_logic @@ Parser.parse Lexer.token lexbuf in
  let rs, vs  = Term.preallocate term in
  let term    = Term.inj term in
  let state   = MemState.inj @@ MemState.preallocate rs vs in
  let stream  = Sem.(
   run qr (fun q  r  -> (term, state) -->* (q, r))
          (fun qs rs -> Stream.zip (prj_stream qs) (prj_stream rs))
  ) in
  let module S = Set.Make(String) in
  let set = ref S.empty in
  let cnt = ref 0 in
  let handler (t, s) =
    let answer = Term.pprint @@ Term.to_logic t in
    let set'   = S.add answer !set in
    cnt := !cnt + 1;
    set := set';
    Printf.printf "\n%d: %s" !cnt answer
  in
  let _ = Printf.printf "\n\nTest program:%s\nOutput:" prog in
  let _ = match n with
    | Some n -> List.iter handler @@ fst @@ Stream.retrieve ~n:n stream
    | None   -> Stream.iter handler stream
  in
  assert_lists expected (S.elements !set) ~printer:show ~cmp:(=)

(* let test_prog_synthesis ?n sem prog expected test_ctx =
  let show s  = "Outcome is not found among answers: " ^ s in
  let lexbuf  = Lexing.from_string prog in
  let part_term = Parser.parse_partial Lexer.token lexbuf in
  (* let term' = const 1 in *)
  let rs, vs  = ["r2"], [] in
  let state   = MemState.preallocate rs vs in
  let stream  =
   run q  (fun q  ->
            fresh (x1)
              (spaceo sem (seq q (var !!"r2")) (MemState.inj state) (const (Nat.inj @@ Nat.of_int 1)) x1)
              (* (MemState.set_localo (MemState.inj state) x1 pathn (!!"r2") (Nat.inj @@ Nat.of_int 1)) *)
          )
          (fun qs -> Stream.map (fun r -> try r#prj with Not_a_value -> T.Stuck) qs)
  in
  let module S = Set.Make(String) in
  let set = ref S.empty in
  let cnt = ref 0 in
  let handler t =
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
  assert_lists expected (S.elements !set) ~printer:show ~cmp:(=) *)
