open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Utils
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Lang.Register
open Relcppmem.Lang.Loc

let sb_prog = <:cppmem<
  spw {{{
      x_sc := 1;
      ret y_sc
  |||
      y_sc := 1;
      ret x_sc
  }}}
>>

module M = MemoryModel.SequentialConsistent

let pprint t =
  let module T = Trace(M) in
  T.trace Format.std_formatter t;
  Format.fprintf Format.std_formatter "----------------------------------@;"

let t = M.init sb_prog ~regs:[] ~locs:[loc "x"; loc "y"]

let () =
  run q
    (fun q  -> M.evalo t q)
    (fun qs -> Stream.iter pprint qs)
