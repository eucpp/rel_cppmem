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

let rec expr_tplo e = conde [
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
      fresh (x mo e v)
        (loco x)
        (t === store mo x e)
        (e === const v)
        (conde [
          (v === integer 1);
          (v === integer 0);
        ]);

      fresh (mo x r)
        (loco x)
        (r === reg "r1")
        (t === repeat (load mo x r) (var r));

      (* fresh (e t1 t2)
        (t === if' e t1 t2)
        (expr_tplo e)
        (seq_stmto ~loco t1)
        (seq_stmto ~loco t2); *)

    ] and seq_stmto ?(loco=fun x -> success) t = conde [
      (stmto ~loco t);
      (* fresh (t1 t2)
        (t === seq t1 t2)
        (stmto ~loco t1)
        (seq_stmto ~loco t2) *)
    ]

let mp_sketch = fun h1 h2 -> <:cppmem<
    spw {{{
        (* r1 := x_na; *)
        m_na := 1;
        (* f_rel := 1 *)
        ? h1
    |||
        (* repeat r1 := f_acq until r1; *)
        ? h2;
        r2 := m_na
    }}}
>>

let mp_tplo t =
  fresh (h1 h2)
    (t === mp_sketch h1 h2)
    (seq_stmto ~loco:((===) (loc "f")) h1)
    (seq_stmto ~loco:((===) (loc "f")) h2)

let _ =
  let progs = Stream.take ~n:1 @@
  run q
    (fun q  ->
      fresh (t h1 h2 i o p s v)
        (seq_stmto ~loco:((===) (loc "f")) h1)
        (seq_stmto ~loco:((===) (loc "f")) h2)
        (q === t)
        (t === mp_sketch h1 h2)
        (i === ReleaseAcquire.State.mem @@ ReleaseAcquire.Memory.init ~regs:["r1"; "r2"] ~mem:[("x", 0); ("y", 0); ("f", 0); ("m", 0)])
        (o === ReleaseAcquire.State.mem s)
        (* (ReleaseAcquire.Memory.shapeo s [loc "x"; loc "y"; loc "f"; loc "m"])
        (ReleaseAcquire.Memory.checko s (loc "x") v)
        (ReleaseAcquire.Memory.checko s (loc "y") v) *)
        (ReleaseAcquire.intrpo t i o)
      ?~(fresh (o e m)
          (o === ReleaseAcquire.State.error e m)
          (ReleaseAcquire.intrpo t i o)
        )
    )
    (fun qs -> qs)
    (* Query.synth
      ~positive:
      [ (fun i o ->
          fresh (p s v)
            (i === ReleaseAcquire.State.init ~regs:["r1"; "r2"] ~mem:[("x", 0); ("y", 0); ("f", 0); ("m", 0)])
            (o === ReleaseAcquire.Node.cfg p s)
            (ReleaseAcquire.State.shapeo s [loc "x"; loc "y"; loc "f"; loc "m"])
            (ReleaseAcquire.State.checko s (loc "x") v)
            (ReleaseAcquire.State.checko s (loc "y") v))
      ; (fun i o ->
          fresh (p s v)
            (i === ReleaseAcquire.State.init ~regs:["r1"; "r2"] ~mem:[("x", 1); ("y", 0); ("f", 0); ("m", 0)])
            (o === ReleaseAcquire.Node.cfg p s)
            (ReleaseAcquire.State.checko s (loc "x") v)
            (ReleaseAcquire.State.checko s (loc "y") v))
      ]
      ~negative: [ fun i o ->
        fresh (p s)
          (i === ReleaseAcquire.State.init ~regs:["r1"; "r2";] ~mem:[("x", 0); ("y", 0); ("f", 0); ("m", 0)])
          (o === ReleaseAcquire.Node.cfg p s)
          (p === stuck ())
          (* (ReleaseAcquire.State.checko s (loc "x") (integer 1)) *)
      ]
      ReleaseAcquire.intrpo mp_tplo *)
      (* ~negative: fun  *)
      (* ~negative: [fun i o -> success] *)
  in
  let module Trace = Utils.Trace(Lang.Term) in
  Format.fprintf Format.std_formatter "Result@;";
  List.iter (Trace.trace Format.std_formatter) progs
  (* Trace.trace Format.std_formatter prog *)

let tests =
  "Synth">::: [
    "RelAcq">::: []
  ]
