open OUnit2
open MiniKanren
open TestUtils
open Lang
open Memory

let test_synth ?n ?(mem_cstrs=[]) prog expected test_ctx =
  let module Sem = Semantics.Make(Semantics.OperationalStep) in
  let show s  = s in
  let lexbuf  = Lexing.from_string prog in
  let term    = Parser.parse Lexer.token lexbuf in
  let rs, vs  = Term.preallocate term in
  let state   = MemState.inj @@ MemState.preallocate rs vs in
  let stream  = Sem.(
   run_with_env q
    (fun env q ->
      let term = Lang.Term.inj_logic (Mapping.create env) term in
      fresh (term' state')
        (q === term)
        (conde @@ List.map (fun cstro -> cstro state') mem_cstrs)
        ((term, state) -->* (term', state'))
    )
    (fun qs -> Stream.map (fun rr -> rr#refine Term.reify ~inj:Term.to_logic) qs)
  ) in
  let module S = Set.Make(String) in
  let set = ref S.empty in
  let cnt = ref 0 in
  let handler t =
    let answer = Term.pprint t in
    let set'   = S.add answer !set in
    cnt := !cnt + 1;
    set := set';
    Printf.printf "\n%d)\n%s" !cnt answer
  in
  let _ = Printf.printf "\n\nTest program: %s\nOutput:" prog in
  let _ = match n with
    | Some n -> List.iter handler @@ fst @@ Stream.retrieve ~n:n stream
    | None   -> Stream.iter handler stream
  in
  let actual = S.elements !set in
  assert_lists expected actual ~printer:show ~cmp:(=)

let prog_ASGN = "?1; ret r1"

let test_ASGN = test_synth ~n:6 prog_ASGN ["r1 := 1;\nr1"]
                ~mem_cstrs:[
                  fun mem -> MemState.get_localo mem (Path.pathn ()) !!"r1" (inj_nat 1)
                ]

let tests =
  "Synthesis">::: [
    "ASGN">:: test_ASGN;
  ]
