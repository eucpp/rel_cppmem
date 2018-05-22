open MiniKanren

open Relcppmem.Lang
open Relcppmem.Lang.Expr
open Relcppmem.Lang.Stmt
open Relcppmem.Lang.Loc
open Relcppmem.Lang.Reg
open Relcppmem.Lang.Value

(* MP relaxed *)

let mp_prog = <:cppmem_par<
  spw {{{
    x_rlx := 1;
    f_rlx := 1
  |||
    repeat
      r1 := f_rlx
    until (r1);
    r2 := x_rlx
  }}}
>>

open Relcppmem.Operational

module IntrpSRA = Interpreter(RelAcq)

let regs = ["r1"; "r2"]
let locs = ["x"; "f"]
let istate = IntrpSRA.State.alloc_istate ~regs ~locs mp_prog

let res = IntrpSRA.eval istate

module Trace = Relcppmem.Utils.Trace(IntrpSRA.State)

let () =
  Format.printf "Results of mp_prog:@\n";
  Stream.iter (Format.printf "%a@;" Trace.trace) res

(* MP with release-acquire synchronization *)

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

let istate = IntrpSRA.State.alloc_istate ~regs ~locs mp_ra_prog

let res = IntrpSRA.eval istate

let () =
  Format.printf "@\nResults of mp_ra_prog:@\n";
  Stream.iter (Format.printf "%a@;" Trace.trace) res
