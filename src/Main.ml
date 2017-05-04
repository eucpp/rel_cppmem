open OUnit2
open MiniKanren
open TestUtils
open Lang
open Memory

let empty_cstrs = [fun _ -> success]

let test_synth ~n ?(mem_allowed=empty_cstrs) ?(mem_forbidden=empty_cstrs) ?(holes_cstrs=empty_cstrs) prog =
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
        (conde @@ List.map (fun cstro -> cstro state') mem_allowed)
        (* (negation @@
          fresh (term'' state'')
            ((term, state) -->* (term'', state''))
            (conde @@ List.map (fun cstro -> cstro state'') mem_forbidden)
        ) *)
    )
    (fun qs -> Stream.map (fun rr -> rr#refine Term.reify ~inj:Term.to_logic) qs)
  ) in
  let tbl = Hashtbl.create n in
  let cnt = ref n in
  let handler t =
    let answer = Term.pprint t in
    if not @@ Hashtbl.mem tbl t then (
      cnt := !cnt - 1;
      Hashtbl.add tbl t answer;
      Printf.printf "\n---------------------------------\n";
      Printf.printf "%s" answer;
      Printf.printf "\n---------------------------------\n"
    )
  in
  let rec loop s =
    if (!cnt != 0) && (not @@ Stream.is_empty s) then (
      handler @@ Stream.hd s;
      loop (Stream.tl s)
    )
  in
  Printf.printf "\n\nTest program: %s" prog;
  loop stream

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

let test_MP () = test_synth ~n:1 prog_MP_part
                ~holes_cstrs:[
                  fun m ->
                    (well_termo (Mapping.get m 1)) &&&
                    (well_termo (Mapping.get m 2))
                ]
                ~mem_allowed:[
                  fun mem ->
                    (MemState.last_valueo mem !!"x" (inj_nat 1)) &&&
                    (MemState.last_valueo mem !!"f" (inj_nat 1))
                ]
                ~mem_forbidden:[
                  fun mem ->
                    (MemState.last_valueo mem !!"x" (inj_nat 0))
                ]

let prog_test = "
    x_rlx := 0;
    y_rlx := 0;
    z_rlx := 0;
    f_rlx := 0;
    spw {{{
        x_rlx := 1;
        f_rel := 1;
    |||
        r1 := f_acq;
        r2 := x_rlx;
        y_rlx := r1;
        z_rlx := r2
    }}}"

let test_test () = test_synth ~n:1 prog_MP_part
                (* ~holes_cstrs:[
                  fun m ->
                    (well_termo (Mapping.get m 1)) &&&
                    (well_termo (Mapping.get m 2))
                ] *)
                ~mem_allowed:[
                  fun mem -> conde [
                    (MemState.last_valueo mem !!"y" (inj_nat 0));

                    (MemState.last_valueo mem !!"y" (inj_nat 1)) &&&
                    (MemState.last_valueo mem !!"z" (inj_nat 1))
                  ]

                ]
                (* ~mem_forbidden:[
                  fun mem ->
                    (MemState.last_valueo mem !!"x" (inj_nat 0))
                ] *)

let _ = test_test ()
