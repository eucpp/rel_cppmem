open MiniKanren

open Relcppmem.Lang
open Relcppmem.Lang.Expr
open Relcppmem.Lang.Stmt
open Relcppmem.Lang.Loc
open Relcppmem.Lang.Reg
open Relcppmem.Lang.Value

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

let prop1 = Prop.(2%"r2" = 0)
let prop2 = Prop.(2%"r2" = 1)

let () =
  let res = run q
    (fun q ->
      fresh (s1 s2)
        (IntrpSRA.evalo ~prop:prop1 istate s1)
        (IntrpSRA.evalo ~prop:prop2 istate s2)
    )
    (fun ss -> ss)
  in
  if not @@ Stream.is_empty res
  then Format.printf "Success@\n"
  else Format.printf "Fail@\n"
