open OUnit2
open MiniKanren
open TestUtils
open Lang
open Memory

let test_synth ?n ?(mem_cstrs=[fun s -> success]) ?(holes_cstrs=[fun m -> success]) prog expected test_ctx =
  let module Sem = Semantics.Make(Semantics.OperationalStep) in
  let parse s =
    let lexbuf  = Lexing.from_string s in
    Parser.parse Lexer.token lexbuf
  in
  let term    = parse prog in
  let rs, vs  = Term.preallocate term in
  let state   = MemState.inj @@ MemState.preallocate rs vs in
  let stream  = Sem.(
   run_with_env q
    (fun env q ->
      let mapping = Mapping.create env in
      let term = Lang.Term.inj_logic mapping term in
      fresh (term' state')
        (q === term)
        (conde @@ List.map (fun cstro -> cstro mapping) holes_cstrs)
        ((term, state) -->* (term', state'))
        (conde @@ List.map (fun cstro -> cstro state') mem_cstrs)
    )
    (fun qs -> Stream.map (fun rr -> rr#refine Term.reify ~inj:Term.to_logic) qs)
  ) in
  let tbl = Hashtbl.create 31 in
  let actual = ref [] in
  let cnt = ref 0 in
  let handler t =
    let answer = Term.pprint t in
    if not @@ Hashtbl.mem tbl t then
      cnt := !cnt + 1;
      actual := t::(!actual);
      Hashtbl.add tbl t answer;
      Printf.printf "\n---------------------------------\n";
      Printf.printf "%s" answer;
      Printf.printf "\n---------------------------------\n";
  in
  let _ = Printf.printf "\n\nTest program: %s" prog in
  let _ = match n with
    | Some n -> List.iter handler @@ fst @@ Stream.retrieve ~n:n stream
    | None   -> Stream.iter handler stream
  in
  let expected = List.map parse expected in
  assert_lists expected !actual ~printer:Term.pprint ~cmp:(=)

let test_ASGN = test_synth ~n:1 "?1; ret r1" ["r1 := 1; ret r1"]
                ~mem_cstrs:[
                  fun mem -> MemState.get_localo mem (Path.pathn ()) !!"r1" (inj_nat 1)
                ]

let well_expro e = Term.(conde[
  fresh (n)
    (e === const n);
  fresh (x)
    (e === var x);
  fresh (op e1 e2 n x)
    (e  === binop op e1 e2)
    (e1 === var x)
    (e2 === const n);
  ])

let well_termo t = Term.(conde [
    fresh (mo x)
      (t === read mo x);
    fresh (mo x e)
      (t === write mo x e)
      (well_expro e);
    (well_expro t);
  ])

let prog_MP = "
    x_rlx := 0;
    f_rlx := 0;
    spw {{{
        x_rlx := 1;
        f_rel := 1;
        ret 1
    |||
        repeat f_acq end;
        r2 := x_rlx;
        ret r2
    }}}"

let prog_MP_part = "
    x_rlx := 0;
    f_rlx := 0;
    spw {{{
        x_rlx := 1;
        ?1;
        ret 1
    |||
        repeat ?2 end;
        r2 := x_rlx;
        ret r2
    }}}"

(* let test_MP = test_synth ~n:2 prog_MP_part [prog_MP]
              ~holes_cstrs:[
                fun m ->
                  (well_termo (Mapping.get m 1)) &&&
                  (well_termo (Mapping.get m 2))
              ]
              ~mem_cstrs:[
                fun mem ->
                  (MemState.last_valueo mem !!"x" (inj_nat 1)) &&&
                  (MemState.last_valueo mem !!"f" (inj_nat 1))
              ] *)

let tests =
  "Synthesis">::: [
    "ASGN">:: test_ASGN;
    (* "MP">:: test_MP; *)
  ]
