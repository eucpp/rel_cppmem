open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Utils
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Lang.Register
open Relcppmem.Lang.Loc
open Relcppmem.Lang.Value

let mp_sketch = fun h1 h2 -> <:cppmem<
    spw {{{
        x_na := 1;
        ? h1
    |||
        ? h2;
        ret x_na
    }}}
>>

module M = MemoryModel.ReleaseAcquire

let pprint =
  let module T = Trace(M) in
  T.trace Format.std_formatter

let regs = []
let locs = [loc "x"; loc "f"]

let () =
  run q
    (fun q  ->
      fresh (rc q1 h1 h2 mo1 mo2 state1)
        (Term.seq_stmto h1)
        (Term.seq_stmto h2)
        (q  === M.init (mp_sketch h1 h2) ~regs ~locs)
        (q1 === M.terminal (const @@ integer 1) state1)
        (M.evalo q q1)
      ?~(fresh (q2 rc state2)
          (q2 === M.terminal rc state2)
          (rc =/= const @@ integer 1)
          (* (rc === stuck ()) *)
          (M.evalo q q2)
        )
    )
    (fun qs -> List.iter pprint @@ Stream.take ~n:1 qs)

let () =
  Format.fprintf Format.std_formatter "@?";
  MiniKanren.report_counters ()
