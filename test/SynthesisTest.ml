open OUnit2
open MiniKanren
open TestUtils
open Lang
open Lang.Term
open Memory

let rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.NonAtomic.all @ Rules.RelAcq.all

module RelAcqStep = (val Rules.make_reduction_relation rules)

module Sem = Semantics.Make(RelAcqStep)

let ret n = const @@ Nat.inj @@ Nat.of_int n

let varo e = Term.(conde [
  fresh (x)
    (e === var x);
  fresh (mo x)
    (* (mo =/= !!MemOrder.NA) *)
    (e === read mo !!"f");
])

let well_expro e = Term.(conde[
  (varo e);

  fresh (n)
    (e === const n);

  fresh (op e1 e2 n x)
    (e  === binop op e1 e2)
    (e1 === var x)
    (e2 === const n);
  ])

let rec well_termo t = Term.(conde [
  (well_expro t);

  fresh (mo x e)
    (t === write mo !!"f" (ret 1))
    (* (mo =/= !!MemOrder.NA) *)
    (well_expro e);

  (* fresh (x l r)
    (t === asgn l r)
    (l === var x)
    (well_expro r); *)

  (* fresh (cond t1 t2)
    (t === if' cond t1 t2)
    (well_expro cond)
    (well_termo t1)
    (well_termo t2); *)

  fresh (t')
    (t === repeat t')
    (well_expro t');
])

let prog_ASGN = fun q -> <:cppmem<
  ? q;
  ret r1
>>

let prog_MP = fun q r -> <:cppmem<
    (* x_na := 0;
    f_na := 0; *)
    spw {{{
        x_na := 42;
        ? q
    |||
        ? r;
        r1 := x_na;
        ret r1
    }}}
>>

let tests =
  "Synthesis">::: [
    "ASGN">:: (fun text_ctx ->
      let term = prog_ASGN in
      let state = MemState.inj @@ MemState.preallocate ["r1"] [] in
      let stream = Sem.(
        run q
          (fun q  ->
            fresh (term' state')
              (well_termo q)
              ((term q, state) -->* (ret 1, state'))
          )
          (fun qs -> Stream.map Term.refine qs)
      ) in
      let printer t =
        Printf.printf "\n---------------------------------\n";
        Printf.printf "%s" @@ Term.pprint t;
        Printf.printf "\n---------------------------------\n";
      in
      List.iter printer @@ Stream.take ~n:9 stream
    );

    "MP">: OUnitTest.TestCase (OUnitTest.Long, fun text_ctx ->
      let term = prog_MP in
      let state = MemState.inj @@ MemState.preallocate ["r1"] ["x"; "f"] in
      let refine = Stream.map Term.refine in
      let stream = Sem.(
        run qr
          (fun q  r  ->
            fresh (state')
              (well_termo q)
              (well_termo r)
              ((term q r, state) -->* (ret 42, state'))
              (negation (
                fresh (term')
                  (term' =/= ret 42)
                  ((term q r, state) -->* (term', state'))
              ))
          )
          (fun qs rs -> Stream.zip (refine qs) (refine rs))
      ) in
      let printer (q, r) =
        Printf.printf "\n---------------------------------\n";
        Printf.printf "q: %s;\nr: %s" (Term.pprint q) (Term.pprint r);
        Printf.printf "\n---------------------------------\n";
      in
      List.iter printer @@ Stream.take ~n:1 stream
    )
  ]
