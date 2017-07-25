open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Memory

let rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.SC.all

module SCStep = (val Rules.make_reduction_relation rules)

module Sem = Semantics.Make(SCStep)

let cond_expr_hinto e = conde [
  fresh (x)
    (e === read !!MemOrder.SC x)
    (x =/= !!"x");

  fresh (x n)
    (e  === binop !!"=" (read !!MemOrder.SC x) (const n))
    (x =/= !!"x")
    (conde [
      (n === Nat.one);
      (n === Nat.zero);
    ])
]

let write_const_hinto t =
  fresh (x n)
    (t === write !!MemOrder.SC x (const n))
    (x =/= !!"x")
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

let term_hinto t = Trace.(trace one) (fun t -> Listener.Goal ("hinto", [Term.pprint @@ Term.refine t])) t (
  (seq_stmt_hinto t) &&& (Trace.(trace one) (fun t -> Listener.Answer ("hinto", [Term.pprint @@ Term.refine t])) t success)
)

let prog_MP = fun h1 h2 -> <:cppmem<
    spw {{{
        x_sc := 1;
        ? h1
    |||
        ? h2;
        ret x_sc
    }}}
>>

(* let prog_MP = <:cppmem<
  spw {{{
    x_sc := 1;
    f_sc := 1
  |||
    if 0
    then f_sc := 0
    else f_sc := 2
    fi;
    ret x_sc
  }}}
>> *)


let ret n = const @@ Nat.inj @@ Nat.of_int n

let _ =
  let logger = TreeLogger.create () in
  let state = MemState.inj @@ MemState.preallocate [] ["x"; "f"] in
  let stream = Sem.(
    run ~listener:(logger :> Listener.t) q
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
    (* run qr
      (fun q  r  -> (prog_MP, state) -->* (q, r))
      (fun qs rs -> Stream.zip (Stream.map (Term.refine) qs) (Stream.map (MemState.refine) rs)) *)
  ) in
  let printer prog =
    Printf.printf "\n---------------------------------\n";
    Printf.printf "prog: %s\n" (Term.pprint prog);
    Printf.printf "\n---------------------------------\n";
  in
  (* let printer (q, r) =
    Printf.printf "\n---------------------------------\n";
    Printf.printf "q: %s\nr: %s\n" (Term.pprint q) (MemState.pprint r);
    Printf.printf "\n---------------------------------\n";
  in *)
  List.iter printer @@ Stream.take ~n:1 stream;
  MiniKanren.report_counters ();
  logger#print ~show_unif:false Format.std_formatter
