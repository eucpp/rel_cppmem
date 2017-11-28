open OUnit2
open MiniKanren
open MiniKanrenStd

open Lang
open Lang.Term
open Lang.Expr
open Lang.Loc
open Lang.Register
open Lang.Value
open MemoryModel

let expr_tplo e = conde [
  fresh (v)
    (e === const v);

  fresh (r)
    (e === var r);

  fresh (op e1 e2)
    (e === binop op e1 e2)
    (expr_tplo e1)
    (expr_tplo e2);
]

let rec stmto ?(loco= fun x -> success) t = conde [
      fresh (x mo n e v)
        (loco x)
        (t === store mo x e)
        (e === const v)
        (conde [
          (v === integer 1);
          (v === integer 0);
        ]);

      fresh (mo x)
        (loco x)
        (t === repeat mo x);

      fresh (e t1 t2)
        (t === if' e t1 t2)
        (expr_tplo e)
        (seq_stmto ~loco t1)
        (seq_stmto ~loco t2);

    ] and seq_stmto ?(loco=fun x -> success) t = conde [
      (stmto ~loco t);
      fresh (t1 t2)
        (t === seq t1 t2)
        (stmto ~loco t1)
        (seq_stmto ~loco t2)
    ]

let mp_sketch = fun h1 h2 -> <:cppmem<
    spw {{{
        x_na := 1;
        ? h1
    |||
        ? h2;
        r1 := x_na;
        y_na := r1
    }}}
>>

let mp_tplo t =
  fresh (h1 h2)
    (t === mp_sketch h1 h2)
    (seq_stmto h1)
    (seq_stmto h2)

let _ =
  Query.synth
    ~interpo:ReleaseAcquire.intrpo
    ~tplo:mp_tplo
    ~positive:[ fun i o ->
      (i === ReleaseAcquire.State.init ~regs:[])
    
    ]
