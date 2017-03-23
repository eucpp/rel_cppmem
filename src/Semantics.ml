open MiniKanren
open Lang

type rule = (Lang.Context.ti -> Lang.Term.ti -> Memory.MemState.ti ->
             Lang.Context.ti -> Lang.Term.ti -> Memory.MemState.ti -> MiniKanren.goal)

type t = (string * rule) list

let make rules = rules

module T = Lang.Term
module C = Lang.Context

let (!) = (!!)

let stepo rls t s t' s' =
  fresh (c c' rdx rdx')
    (splito t c rdx)
    (conde [
      (reducibleo rdx !false) &&& (rdx === rdx') &&& (c === c') &&& (t =/= t') &&& (s === s');
      (reducibleo rdx !true ) &&& (rdx =/= rdx') &&& (conde @@ (List.map (
          fun (name, rule) -> (rule c rdx s c' rdx' s') (*&&& (rl === inj name)*)
        ) rls));
      ])
    (plugo t' c' rdx')


let rec spaceo rls t s t'' s'' =
  conde [
    (reducibleo t !false) &&& (t === t'') &&& (s === s'');
    (fresh (t' s' rl)
      (reducibleo t !true)
      (stepo rls t s t' s')
      (spaceo rls t' s' t'' s''));
  ]
