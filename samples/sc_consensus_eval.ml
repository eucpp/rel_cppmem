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
      r1 := CAS(sc, sc, x, 0, 1);
      if r1 then
        ret 0
      else
        ret 1
      fi
  |||
    r2 := CAS(sc, sc, x, 0, 1);
    if r2 = 0 then
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

let t = M.init consensus_prog ~regs:[reg "r1"; reg "r2"] ~locs:[loc "x";]

let () =
  run q
    (fun q  -> M.evalo t q)
    (fun qs -> Stream.iter pprint qs)
