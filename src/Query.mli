type ('at, 'bt, 'al, 'bl) assertion =
  ('at, 'al) Semantics.Input.ti -> ('bt, 'bl) Semantics.Output.ti -> MiniKanren.goal

val exec :
  ('at, 'bt, 'ct, 'al, 'bl, 'cl) Semantics.interpreter ->
  ('at, 'al) Semantics.Prog.ti -> ('bt, 'bl) Semantics.Input.ti ->
  ('ct, 'cl) Semantics.Output.reified MiniKanren.Stream.t

val angelic :
  ('at, 'bt, 'ct, 'al, 'bl, 'cl) Semantics.interpreter ->
  ('bt, 'bl) Semantics.tpred -> ('ct, 'cl) Semantics.tpred ->
  ('at, 'al) Semantics.Prog.ti ->
  (('bt, 'bl) Semantics.Input.reified * ('ct, 'cl) Semantics.Input.reified) MiniKanren.Stream.t

val verify :
  ('at, 'bt, 'ct, 'al, 'bl, 'cl) Semantics.interpreter ->
  ('bt, 'bl) Semantics.tpred -> ('bt, 'ct, 'bl, 'cl) assertion ->
  ('at, 'al) Semantics.Prog.ti ->
  (('bt, 'bl) Semantics.Input.reified * ('ct, 'cl) Semantics.Input.reified) MiniKanren.Stream.t

val synth :
  ?positive: (('bt, 'bl) MiniKanren.injected -> ('ct, 'cl) MiniKanren.injected -> MiniKanren.goal) list ->
  ?negative: (('bt, 'bl) MiniKanren.injected -> ('ct, 'cl) MiniKanren.injected -> MiniKanren.goal) list ->
  ('at, 'bt, 'ct, 'al, 'bl, 'cl) Semantics.interpreter ->
  ('at, 'al) Semantics.tpred ->
  ('at, 'al) Semantics.Prog.reified MiniKanren.Stream.t


(* val synth :
  ('at, 'bt, 'ct, 'al, 'bl, 'cl) Semantics.interpreter ->
  ('bt, 'bl) Semantics.tpred -> ('bt, 'ct, 'bl, 'cl) assertion ->
  ('at, 'al) Semantics.Prog.ti -> unit *)
