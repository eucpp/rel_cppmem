open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Utils
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Lang.Register
open Relcppmem.Lang.Loc

let consensus_prog = <:cppmem<
  spw {{{
      if CAS(sc, sc, x, 0, 1) then
        ret 0
      else
        ret 1
      fi
  |||
    if CAS(sc, sc, x, 0, 1) = 0 then
      ret 0
    else
      ret 1
    fi
  }}}
>>

module M = MemoryModel.SequentialConsistent

let pprint t =
  let module T = Trace(M) in
  T.trace Format.std_formatter t;
  Format.fprintf Format.std_formatter "----------------------------------@;"

let t = M.init consensus_prog ~regs:[] ~locs:[loc "x";]

let () =
  run q
    (fun q  -> M.evalo t q)
    (fun qs -> Stream.iter pprint qs)
