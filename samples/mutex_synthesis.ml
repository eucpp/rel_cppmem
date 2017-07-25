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
    (e === read !!MemOrder.SC x);

  fresh (x n)
    (e  === binop !!"=" (read !!MemOrder.SC x) (const n))
    (conde [
      (* (n === Nat.one); *)
      (n === Nat.zero);
    ])
]

let write_const_hinto t =
  fresh (x n)
    (t === write !!MemOrder.SC x (const n))
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

let cond_expr_hinto t = Trace.(trace one) (fun t -> Listener.Goal ("hinto", [Term.pprint @@ Term.refine t])) t (
  (cond_expr_hinto t) &&& (Trace.(trace one) (fun t -> Listener.Answer ("hinto", [Term.pprint @@ Term.refine t])) t success)
)

let term_hinto t = Trace.(trace one) (fun t -> Listener.Goal ("hinto", [Term.pprint @@ Term.refine t])) t (
  (seq_stmt_hinto t) &&& (Trace.(trace one) (fun t -> Listener.Answer ("hinto", [Term.pprint @@ Term.refine t])) t success)
)

let prog_MUTEX = fun h1 h2 h3 h4 -> <:cppmem<
    spw {{{
        ? h1;
        if ? h2 then
          ret 1
        else
          ret 0
        fi
    |||
        ? h3;
        if ? h4 then
          ret 1
        else
          ret 0
        fi
    }}}
>>

(* let prog_MUTEX = <:cppmem<
    spw {{{
        x_sc := 0;
        if x_sc then
          ret 1
        else
          ret 0
        fi
    |||
        x_sc := 1;
        if x_sc != 1 then
          ret 1
        else
          ret 0
        fi
    }}}
>> *)

let int_of_bool b = if b then 1 else 0

let ret n = const @@ Nat.inj @@ Nat.of_int n

let pair (x, y) = pair (ret x) (ret y)

let _ =
  let logger = TreeLogger.create () in
  let term = prog_MUTEX in
  let state = MemState.inj @@ MemState.preallocate [] ["x"; "y";] in
  let stream = Sem.(
    run ~listener:(logger :> Listener.t) q
      (fun prog ->
        fresh (h1 h2 h3 h4 state1 state2)
          (term_hinto h1)
          (cond_expr_hinto h2)
          (term_hinto h3)
          (cond_expr_hinto h4)
          (prog === term h1 h2 h3 h4)
          ((prog, state) -->* (pair (1, 0), state1))
          ((prog, state) -->* (pair (0, 1), state2))
          (negation (
            fresh (state')
              ((prog, state) -->* (pair (1, 1), state'))
          ))
      )
      (fun progs -> Stream.map (Term.refine) progs)
      (* (fun progs ->
        let pred prog = Sem.(
          run q
            (fun q ->
              fresh (state')
                (q === pair (1, 1))
                ((Term.inj prog, state) -->* (q, state'))
            )
            (fun qs -> Stream.is_empty qs)
        ) in
        Stream.filter pred @@ Stream.map (fun rr -> rr#prj) progs
      ) *)
    (* run qr
      (fun q r ->
        fresh (s1 s2)
          ((prog_MUTEX, state) -->* (q, r)) *)
          (* ((prog_MUTEX, state) -->* (pair (1, 0), s1))
          ((prog_MUTEX, state) -->* (pair (0, 1), s2))
          (negation (
            fresh (state')
              ((prog_MUTEX, state) -->* (pair (1, 1), state'))
          ))
      ) *)
      (* (fun qs rs -> Stream.zip (Stream.map (Term.refine) qs) (Stream.map (MemState.refine) rs)) *)
  ) in
  let printer prog =
  (* let printer (q, r) = *)
    Printf.printf "\n---------------------------------\n";
    Printf.printf "prog: %s\n" (Term.pprint prog);
    (* Printf.printf "\n%s\n%s\n" (Term.pprint q) (MemState.pprint r); *)
    Printf.printf "\n---------------------------------\n";
  in
  List.iter printer @@ Stream.take ~n:1 stream;
  MiniKanren.report_counters ();
  logger#print ~show_unif:false Format.std_formatter
