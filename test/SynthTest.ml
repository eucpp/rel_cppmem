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
      fresh (x mo n e v)
        (loco x)
        (t === store mo x e)
        (e === const v)
        (conde [
          (v === integer 1);
          (v === integer 0);
        ]);

      fresh (mo x r)
        (loco x)
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
        r1 := x_na;
        m_na := r1;
        (* f_rel := 1 *)
        ? h1
    |||
        (* repeat r1 := f_acq until r1; *)
        ? h2;
        r2 := m_na;
        y_na := r2
    }}}
>>

let prog = <:cppmem<
    spw {{{
        (* r1 := x_na; *)
        x_na := 1;
        f_rel := 1
        (* ? h1 *)
    |||
        repeat r1 := f_acq until r1;
        (* ? h2; *)
        r2 := x_na;
        y_na := r2
    }}}
>>

(* let _ =
  (* let module Trace = Utils.Trace(Lang.Term) in *)
  Stream.iter (fun p -> Format.fprintf Format.std_formatter "%s@;" (Lang.Term.show @@ p#reify Lang.Term.reify)) @@
    run q (fun q -> q === mp_sketch) (fun qs -> qs) in
  Format.fprintf Format.std_formatter "@." *)

(* let _ =
  let module Trace = Utils.Trace(ReleaseAcquire.Node) in
  Stream.iter (Trace.trace Format.std_formatter) @@
    (* (run q (fun q ->
        ?~(fresh (i o p s)
          (i === ReleaseAcquire.State.init ~regs:["r1"; "r2"] ~mem:[("x", 0); ("y", 0); ("f", 0); ("m", 0)])
          (o === ReleaseAcquire.Node.cfg p s)
          (p === stuck ())
          (ReleaseAcquire.intrpo prog i o)
      ))
      (fun qs -> qs)); *)
    Query.exec
      ReleaseAcquire.intrpo
      prog
      (ReleaseAcquire.State.init ~regs:["r1"; "r2"] ~mem:[("x", 0); ("y", 0); ("f", 0); ("m", 0)]);
  Format.fprintf Format.std_formatter "@." *)

let mp_tplo t =
  fresh (h1 h2)
    (t === mp_sketch h1 h2)
    (seq_stmto ~loco:((===) (loc "f")) h1)
    (seq_stmto ~loco:((===) (loc "f")) h2)

let _ =
  let progs = Stream.take ~n:1 @@
    Query.synth
      ~positive:
      [ fun i o ->
          fresh (p s v)
            (i === ReleaseAcquire.State.init ~regs:["r1"; "r2"] ~mem:[("x", 0); ("y", 0); ("f", 0); ("m", 0)])
            (o === ReleaseAcquire.Node.cfg p s)
            (ReleaseAcquire.State.shapeo s [loc "x"; loc "y"; loc "f"; loc "m"])
            (ReleaseAcquire.State.checko s (loc "x") v)
            (ReleaseAcquire.State.checko s (loc "y") v)
      (* ; fun i o ->
          fresh (p s)
            (i === ReleaseAcquire.State.init ~regs:["r1"; "r2"] ~mem:[("x", 0); ("f", 0); ("y", 0)])
            (o === ReleaseAcquire.Node.cfg p s)
            (ReleaseAcquire.State.shapeo s [loc "x"; loc "f"; loc "y"])
            (ReleaseAcquire.State.checko s (loc "y") (integer 1))
      ] *)
      ]
      ~negative: [ fun i o ->
        fresh (p s)
          (i === ReleaseAcquire.State.init ~regs:["r1"; "r2";] ~mem:[("x", 0); ("y", 0); ("f", 0); ("m", 0)])
          (o === ReleaseAcquire.Node.cfg p s)
          (p === stuck ())
          (* (ReleaseAcquire.State.checko s (loc "x") (integer 1)) *)
      ]
      ReleaseAcquire.intrpo mp_tplo
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
