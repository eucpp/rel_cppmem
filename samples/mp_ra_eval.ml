open MiniKanren

open Relcppmem.Lang
open Relcppmem.Lang.Expr
open Relcppmem.Lang.Stmt
open Relcppmem.Lang.Loc
open Relcppmem.Lang.Reg
open Relcppmem.Lang.Value

let mp_ra_prog = <:cppmem_par<
spw {{{
    x_rlx := 1;
    f_rel := 1
  |||
    repeat
      r1 := f_acq
    until (r1);
    r2 := x_rlx
  }}}
  >>

open Relcppmem.Operational

module IntrpSRA = Interpreter(RelAcq)

let regs = ["r1"; "r2"]
let locs = ["x"; "f"]
let istate = IntrpSRA.State.alloc_istate ~regs ~locs mp_ra_prog

let res = IntrpSRA.eval istate

module Trace = Relcppmem.Utils.Trace(IntrpSRA.State)

let () = Stream.iter (Format.printf "%a@;" Trace.trace) res
