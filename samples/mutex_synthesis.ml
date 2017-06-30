open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Memory

let rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.SC.all

module SCStep = (val Rules.make_reduction_relation rules)

module Sem = Semantics.Make(SCStep)

let expr_hinto e = Term.(conde[
  fresh (mo x)
    (e === read mo x);

  fresh (op e1 e2 mo n x)
    (e  === binop op e1 e2)
    (e1 === read mo x)
    (e2 === const n)
    (conde [
      (n === Nat.one);
      (n === Nat.zero);
    ])
    (* (conde [
      (op === !!"=");
      (op === !!"!=");
    ]); *)
  ])

let stmt_hinto t = Term.(conde [

  fresh (mo x n)
    (t === write mo x (const n))
    (conde [
      (n === Nat.one);
      (* (n === Nat.zero); *)
    ]);

  (* fresh (l r x y mo)
    (t === asgn l r)
    (l === var x)
    (conde [
      (r === read !!MemOrder.SC y);
      (r === const Nat.one);
    ]) *)
])

let prog_MUTEX = fun q r s t -> <:cppmem<
    spw {{{
        ? q;
        if ? r then
          ret 1
        else
          ret 0
        fi
    |||
        ? s;
        if ? t then
          ret 1
        else
          ret 0
        fi
    }}}
>>

(* let prog_MUTEX = <:cppmem<
    spw {{{
        y_sc := 1;
        if x_sc != 1 then
          ret 1
        else
          ret 0
        fi
    |||
        x_sc := 1;
        if y_sc = 0 then
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
  let term = prog_MUTEX in
  let state = MemState.inj @@ MemState.preallocate [] ["x"; "y";] in
  let refine = Stream.map Term.refine in
  let stream = Sem.(
    (* run qrst
      (fun q r s t ->
        fresh (prog state1 state2)
          (stmt_hinto q)
          (expr_hinto r)
          (stmt_hinto s)
          (expr_hinto t)
          (prog === term q r s t)
          ((prog, state) -->* (pair (1, 0), state1))
          ((prog, state) -->* (pair (0, 1), state2))
          (negation (
            fresh (state')
              ((prog, state) -->* (pair (1, 1), state'))
          ))
      )
      (fun qs rs ss ts -> Utils.zip4 (refine qs) (refine rs) (refine ss) (refine ts)) *)
    run qr
      (fun q r ->
        fresh (s1 s2)
          ((prog_MUTEX, state) -->* (q, r))
          ((prog_MUTEX, state) -->* (pair (1, 0), s1))
          ((prog_MUTEX, state) -->* (pair (0, 1), s2))
          (negation (
            fresh (state')
              ((prog_MUTEX, state) -->* (pair (1, 1), state'))
          ))
      )
      (fun qs rs -> Stream.zip (refine qs) (Stream.map (MemState.refine) rs))
  ) in
  (* let printer (q, r, s, t) = *)
  let printer (q, r) =
    Printf.printf "\n---------------------------------\n";
    (* Printf.printf "q: %s\nr: %s\ns: %s\nt: %s\n" (Term.pprint q) (Term.pprint r) (Term.pprint s) (Term.pprint t); *)
    Printf.printf "\n%s\n%s\n" (Term.pprint q) (MemState.pprint r);
    Printf.printf "\n---------------------------------\n";
  in
  List.iter printer @@ Stream.take ~n:3 stream
