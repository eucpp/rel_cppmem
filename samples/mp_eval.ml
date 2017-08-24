open MiniKanren
open MiniKanrenStd
open Relcppmem
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Lang.Register
open Relcppmem.Lang.Loc

let mp_prog = <:cppmem<
    spw {{{
        x_na := 1;
        f_rel := 1;
        ret 1
    |||
        repeat f_acq end;
        r2 := x_na;
        ret r2
    }}}
>>

(* let mp_prog = <:cppmem<
    x_na := 1;
    ret 1
>> *)

module Basic = Rules.Basic(Machines.Front)
module ThreadSpawning = Rules.ThreadSpawning(Machines.Front)
module NonAtomic = Rules.NonAtomic(Machines.Front)
module ReleaseAcquire = Rules.ReleaseAcquire(Machines.Front)

let rules = List.concat [
  Basic.all;
  ThreadSpawning.all;
  NonAtomic.all;
  ReleaseAcquire.all;
]


let stepo = Semantics.make_reduction_relation (Semantics.Configuration.lift_splitting splito) (Semantics.Configuration.lift_plugging plugo) rules

let evalo = Semantics.make_eval stepo

let reify = Semantics.Configuration.reify (Term.reify) (Machines.Front.reify)

let inj = Semantics.Configuration.inj (Term.inj) (Machines.Front.inj)

let pprint = function
  | Value c   -> Semantics.(Configuration.(Format.printf "Return: %a\nState:\n %a" Lang.Term.pprint c.T.prog Machines.Front.pprint c.T.state))
  | Var (_,_) -> invalid_arg "Unexpected free variable"

let t = Semantics.Configuration.cfg mp_prog (Machines.Front.preallocate [reg "r1"; reg "r2"] [loc "x"; loc "f"])

let () =
  run q
    (fun q  -> evalo t q)
    (fun qs -> Stream.iter (fun rr -> pprint @@ rr#refine reify ~inj) qs)
