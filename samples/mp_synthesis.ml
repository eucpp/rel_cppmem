open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Memory

let rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.NonAtomic.all @ Rules.RelAcq.all

module RelAcqStep = (val Rules.make_reduction_relation rules)

module Sem = Semantics.Make(RelAcqStep)

let const_hinto t =
  fresh (n)
    (t === const n)

let read_hinto e =
  fresh (mo x)
    (e === read mo x)

let expr_hinto e = conde [
  (read_hinto e);

  (const_hinto e);

  fresh (op e1 e2 n)
    (e  === binop op e1 (const n))
    (read_hinto e1);
]

let write_const_hinto t =
  fresh (mo x n)
    (t === write mo x (const n))

let write_expr_hinto t =
  fresh (mo x e)
    (t === write mo x e)
    (expr_hinto e)

let rec stmt_hinto t = conde [
  (write_const_hinto t);

  (expr_hinto t);

  fresh (t')
    (t === repeat t')
    (expr_hinto t');

  (write_expr_hinto t);

  fresh (t')
    (t === repeat t')
    (seq_stmt_hinto t');

  fresh (cond t1 t2)
    (t === if' cond t1 t2)
    (seq_stmt_hinto t1)
    (seq_stmt_hinto t2);

] and seq_stmt_hinto t = conde [
  (stmt_hinto t);

  fresh (t1 t2)
    (t === seq t1 t2)
    (stmt_hinto t1)
    (conde [
      (stmt_hinto t2);
      (seq_stmt_hinto t2);
    ]);
]

let term_hinto t = conde [expr_hinto t; stmt_hinto t]

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
