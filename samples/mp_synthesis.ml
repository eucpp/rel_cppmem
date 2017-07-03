open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Memory

let rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.NonAtomic.all @ Rules.RelAcq.all

module RelAcqStep = (val Rules.make_reduction_relation rules)

module Sem = Semantics.Make(RelAcqStep)

let varo e = Term.(conde [
  fresh (x)
    (e === var x);
  fresh (mo x)
    (e === read mo x);
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

  fresh (mo x n)
    (t === write mo x (const n));

  fresh (l r x)
    (t === asgn l r)
    (l === var x)
    (well_expro r);

  fresh (t')
    (t === repeat t')
    (well_expro t');

  fresh (cond t1 t2)
    (t === if' cond t1 t2)
    (well_termo t1)
    (well_termo t2);
])

let prog_MP = fun q r -> <:cppmem<
    spw {{{
        x_na := 1;
        ? q
    |||
        ? r;
        r1 := x_na;
        ret r1
    }}}
>>

let ret n = const @@ Nat.inj @@ Nat.of_int n

let _ =
  let term = prog_MP in
  let state = MemState.inj @@ MemState.preallocate ["r1"] ["x"; "f"] in
  let refine = Stream.map Term.refine in
  let stream = Sem.(
    run qr
      (fun q r ->
        fresh (state')
          (well_termo q)
          (well_termo r)
          ((term q r, state) -->* (ret 1, state'))
          (negation (
            fresh (term')
              (term' =/= ret 1)
              ((term q r, state) -->* (term', state'))
          ))
      )
      (fun qs rs -> Stream.zip (refine qs) (refine rs))
  ) in
  let printer (q, r) =
    Printf.printf "\n---------------------------------\n";
    Printf.printf "q: %s\nr: %s\n" (Term.pprint q) (Term.pprint r);
    Printf.printf "\n---------------------------------\n";
  in
  List.iter printer @@ Stream.take ~n:1 stream;
  MiniKanren.report_counters ()
