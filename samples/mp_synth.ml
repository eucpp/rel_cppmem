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
      fresh (q' h1 h2 state)
        (Term.seq_stmto h1)
        (Term.seq_stmto h2)
        (q  === M.init (mp_sketch h1 h2) ~regs ~locs)
        (q' === M.terminal (const @@ integer 1) state)
        (M.evalo q q')
        (negation (
          fresh (rc q'' state'')
            (rc =/= const @@ integer 1)
            (q'' === M.terminal rc state'')
            (M.evalo q q'')
        )))
    (fun qs -> List.iter pprint @@ Stream.take ~n:1 qs)
