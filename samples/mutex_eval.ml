open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Utils
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Lang.Register
open Relcppmem.Lang.Loc

let mutex_prog = <:cppmem<
  spw {{{
      y_sc := 1;
      if x_sc = 0 then
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
>>

module M = MemoryModel.SequentialConsistent

let pprint =
  let module T = Trace(M) in
  T.trace Format.std_formatter

let t = M.init mutex_prog ~regs:[] ~locs:[loc "x"; loc "y"]

let () =
  run q
    (fun q  -> M.evalo t q)
    (fun qs -> Stream.iter pprint qs)
