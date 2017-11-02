open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Utils
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Lang.Register
open Relcppmem.Lang.Loc
open Relcppmem.Lang.Value

let consensus_sketch = fun h1 h2 -> <:cppmem<
    spw {{{
        if ? h1 then
          ret 1
        else
          ret 0
        fi
    |||
        if ? h2 then
          ret 1
        else
          ret 0
        fi
    }}}
>>

module M = MemoryModel.SequentialConsistent

let pprint =
  let module T = Trace(M) in
  T.trace Format.std_formatter

let regs = []
let locs = [loc "x";]

let pair a b = pair (const @@ Value.integer a) (const @@ Value.integer b)

let _ =
  run q
    (fun q ->
      fresh (h1 h2 q1 q2 state1 state2)
        (Term.bool_expro h1)
        (Term.bool_expro h2)
        (q  === M.init (consensus_sketch h1 h2) ~regs ~locs)
        (q1 === M.terminal (pair 1 1) state1)
        (q2 === M.terminal (pair 0 0) state2)
        (M.evalo q q1)
        (M.evalo q q2)
      ?~(fresh (q' rc state')
          (q' === M.terminal rc state')
          (conde [
            (rc === pair 1 0);
            (rc === pair 0 1);
          ])
          (M.evalo q q')
        )
    )
    (fun qs -> List.iter pprint @@ Stream.take ~n:1 qs)

let () =
  Format.fprintf Format.std_formatter "@?";
  MiniKanren.report_counters ()
