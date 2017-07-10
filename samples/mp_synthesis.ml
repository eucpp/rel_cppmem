open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Memory

let rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.NonAtomic.all @ Rules.RelAcq.all

module RelAcqStep = (val Rules.make_reduction_relation rules)

module Sem = Semantics.Make(RelAcqStep)

let cond_expr_hinto e = conde [
  fresh (x mo)
    (e === read mo x);

  fresh (x mo n)
    (e  === binop !!"=" (read mo x) (const n))
    (conde [
      (n === Nat.one);
      (n === Nat.zero);
    ])
]

let write_const_hinto t =
  fresh (x mo n)
    (t === write mo x (const n))
    (conde [
      (n === Nat.one);
      (n === Nat.zero);
    ])

let rec stmt_hinto t = conde [
  (write_const_hinto t);

  fresh (t')
    (t === repeat t')
    (cond_expr_hinto t');

  fresh (t')
    (t === repeat t')
    (seq_stmt_hinto t');

  fresh (e t1 t2)
    (t === if' e t1 t2)
    (cond_expr_hinto e)
    (seq_stmt_hinto t1)
    (seq_stmt_hinto t2);

] and seq_stmt_hinto t = conde [
  (stmt_hinto t);

  fresh (t1 t2)
    (t === seq t1 t2)
    (stmt_hinto t1)
    (seq_stmt_hinto t2);
]

let term_hinto = seq_stmt_hinto

let prog_MP = fun h1 h2 -> <:cppmem<
    spw {{{
        x_na := 1;
        ? h1
    |||
        ? h2;
        ret x_na
    }}}
>>

let ret n = const @@ Nat.inj @@ Nat.of_int n

let _ =
  let state = MemState.inj @@ MemState.preallocate [] ["x"; "f"] in
  let stream = Sem.(
    run q
      (fun prog ->
        fresh (h1 h2 state')
          (term_hinto h1)
          (term_hinto h2)
          (prog === prog_MP h1 h2)
          ((prog, state) -->* (ret 1, state'))
          (negation (
            fresh (term' state')
              (term' =/= ret 1)
              ((prog, state) -->* (term', state'))
          ))
      )
      (fun progs -> Stream.map (Term.refine) progs)
  ) in
  let printer prog =
    Printf.printf "\n---------------------------------\n";
    Printf.printf "q: %s\n" (Term.pprint prog);
    Printf.printf "\n---------------------------------\n";
  in
  List.iter printer @@ Stream.take ~n:1 stream;
  MiniKanren.report_counters ()
