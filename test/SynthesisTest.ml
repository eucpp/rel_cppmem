open OUnit2
open MiniKanren
open TestUtils
open Lang
open Lang.Term
open Rules
open Memory

let test_synth ?n ?(mem_cstrs=[fun s -> success]) ?(holes_cstrs=[fun m -> success]) term expected test_ctx =
  let rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.Rlx.all @ Rules.RelAcq.all in
  let module Step = (val make_reduction_relation rules) in
  let module Sem = Semantics.Make(Step) in
  let rs, vs  = ["r1";"r2";"r3";"r4"], ["x";"y";"z";"f"] in
  let state   = MemState.inj @@ MemState.preallocate rs vs in
  let stream  = Sem.(
   run q
    (fun q ->
      let term = term q in
      fresh (term' state')
        (conde @@ List.map (fun cstro -> cstro q) holes_cstrs)
        ((term, state) -->* (term', state'))
        (conde @@ List.map (fun cstro -> cstro state') mem_cstrs)
    )
    (fun qs -> Stream.map Term.refine qs)
  ) in
  let tbl = Hashtbl.create 31 in
  let actual = ref [] in
  let cnt = ref 0 in
  let handler t =
    let answer = Term.pprint t in
    if not @@ Hashtbl.mem tbl t then
      cnt := !cnt + 1;
      actual := answer::(!actual);
      Hashtbl.add tbl t answer;
      Printf.printf "\n---------------------------------\n";
      Printf.printf "%s" answer;
      Printf.printf "\n---------------------------------\n";
  in
  (* let _ = Printf.printf "\n\nTest program: %s" (Term.pprint @@ Term.to_logic @@ prj (term @@ var ) in *)
  let _ = match n with
    | Some n -> List.iter handler @@ fst @@ Stream.retrieve ~n:n stream
    | None   -> Stream.iter handler stream
  in
  assert_lists expected !actual ~printer:(fun s -> s) ~cmp:(=)

let prog_ASGN = fun q -> <:cppmem< ? q; ret r1 >>

let test_ASGN = test_synth ~n:1 prog_ASGN ["r1 := 1"]
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

let prog_MP_part = fun q r -> <:cppmem<
    x_rlx := 0;
    f_rlx := 0;
    spw {{{
        x_rlx := 1;
        ? q;
        ret 1
    |||
        repeat ? r end;
        r2 := x_rlx;
        ret r2
    }}}
>>

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
