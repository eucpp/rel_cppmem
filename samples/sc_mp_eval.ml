open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Utils
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Lang.Register
open Relcppmem.Lang.Loc

let mp_prog = <:cppmem<
    spw {{{
        x_sc := 1;
        f_sc := 1
    |||
        repeat f_sc end;
        ret x_sc
    }}}
>>

module M = MemoryModel.SequentialConsistent

let pprint =
  let module T = Trace(M) in
  T.trace Format.std_formatter

let t = M.init mp_prog ~regs:[] ~locs:[loc "x"; loc "f"]

let () =
  run q
    (fun q  -> M.evalo t q)
    (fun qs -> Stream.iter pprint qs)
