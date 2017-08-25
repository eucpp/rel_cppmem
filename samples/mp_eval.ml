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
        x_na := 1;
        f_rel := 1
    |||
        repeat f_acq end;
        ret x_na
    }}}
>>

module M = MemoryModel.ReleaseAcquire

let pprint =
  let module T = Trace(M) in
  T.trace Format.std_formatter

let t = M.init mp_prog ~regs:[reg "r1"; reg "r2"] ~locs:[loc "x"; loc "f"]

let () =
  run q
    (fun q  -> M.evalo t q)
    (fun qs -> Stream.iter pprint qs)
