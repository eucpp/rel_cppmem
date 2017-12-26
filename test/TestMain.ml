open OUnit2

open MiniKanren

open Lang
open Lang.Term
open Lang.Expr
open Lang.Loc
open Lang.Register
open Lang.Value
open MemoryModel
open Axiomatic
open Utils

let tests = Test.(
  make_testsuite ~name:"relcppmem" ~tests: [
    (* LitmusTest.tests; *)
    (* SynthTest.tests; *)
  ]
)


(* let () =
  Test.ounit_run tests *)

  (* Test.simple_run tests;
  MiniKanren.report_counters () *)


let prog_LB = <:cppmem<
    spw {{{
        r1 := x_sc;
        y_sc := 1
    |||
        r2 := y_sc;
        x_sc := 1
    }}}
>>

let () =

  let stream = SequentialConsistent.sc_exec prog_LB
    (* run qr (fun q  r  -> SequentialConsistent.sc_execo prog_LB q r)
           (fun qs rs -> Stream.zip qs rs) *)
  in
  Stream.take ~n:10 stream |>
  List.iter (fun (q, r) ->
    let module TraceEs  = Trace(EventSet) in
    let module TraceOrd = Trace(Order) in
    let pp_es  = TraceEs.trace in
    let pp_ord = TraceOrd.trace in
    Format.printf "@[<v>Events:@;<1 2>%a@;Order:@;<1 2>%a@;@]" pp_es q pp_ord r
  )

(* let () =
  let stream =
    run qr (fun q  r  -> SequentialConsistent.sc_execo prog_LB q r)
           (fun qs rs -> Stream.zip qs rs)
  in
  let i = ref 0 in
  Stream.take ~n:1 stream |>
  List.iter (fun (q, r) ->
    let module TraceEID = Trace(EventID) in
    let pp = TraceEID.trace in
    i := !i + 1;
    Format.printf "@[<v>#%d: (%a, %a)@.@]" !i pp q pp r
  ) *)
