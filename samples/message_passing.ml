open MiniKanren
open Relcppmem
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Memory

let prog = <:cppmem<
    spw {{{
        x_rlx := 42;
        f_rel := 1
    |||
        repeat f_acq end;
        r1 := x_rlx;
        ret r1
    }}}
>>

let rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.Rlx.all @ Rules.RelAcq.all

module Step = (val Rules.make_reduction_relation rules)

module Sem = Semantics.Make(Step)

let state = MemState.inj @@ MemState.preallocate ["r1"] ["x";"f"]

let stream = run qr Sem.(fun q  r  -> (prog, state) -->* (q, r))
                        (fun qs rs -> Stream.zip qs rs)

let handler (t, s) =
  let t' = Term.refine t in
  let s' = MemState.refine s in
  Printf.printf "Ret: %s;\n%s" (Term.pprint t') (MemState.pprint s')

let _ =
  Stream.iter handler stream;
  MiniKanren.report_counters ()
