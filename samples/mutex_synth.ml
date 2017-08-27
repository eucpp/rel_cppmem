open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Utils
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Lang.Register
open Relcppmem.Lang.Loc
open Relcppmem.Lang.Value

let mutex_sketch = fun h1 h2 h3 h4 -> <:cppmem<
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

module M = MemoryModel.SequentialConsistent

let pprint =
  let module T = Trace(M) in
  T.trace Format.std_formatter

let regs = []
let locs = [loc "x"; loc "y"]

let pair a b = pair (const @@ Value.integer a) (const @@ Value.integer b)

let _ =
  run q
    (fun q ->
      fresh (h1 h2 h3 h4 q1 q2 state1 state2)
        (Term.seq_stmto h1)
        (Term.bool_expro h2)
        (Term.seq_stmto h3)
        (Term.bool_expro h4)
        (q  === M.init (mutex_sketch h1 h2 h3 h4) ~regs ~locs)
        (q1 === M.terminal (pair 1 0) state1)
        (q2 === M.terminal (pair 0 1) state2)
        (M.evalo q q1)
        (M.evalo q q2)
        (negation (
          fresh (q' state')
            (q' === M.terminal (pair 1 1) state')
            (M.evalo q q')
        ))
    )
    (fun qs -> List.iter pprint @@ Stream.take ~n:1 qs)
