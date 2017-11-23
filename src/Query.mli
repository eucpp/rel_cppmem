type ('at, 'bt, 'al, 'bl') assertion =
  ('at, 'al) Semantics.Input.ti -> ('bt, 'bl) Semantics.Output.ti -> MiniKanren.goal

val exec :
  ('at, 'bt, 'ct, 'al, 'bl, 'cl) interpreter ->
  ('at, 'al) Semantics.Prog.ti -> ('bt, 'bl) Semantics.Input.ti -> ('ct, 'cl) Output.reified MiniKanren.Stream.t

val verify :
  ('at, 'bt, 'ct, 'al, 'bl, 'cl) interpreter -> ('bt, 'bl) tpred -> ('bt, 'ct, 'bl, 'cl') assertion ->
  ('at, 'al) Semantics.Prog.ti -> ('bt, 'bl) Input.reified MiniKanren.Stream.t

val synth :
  ('at, 'bt, 'ct, 'al, 'bl, 'cl) interpreter -> ('bt, 'bl) tpred -> ('bt, 'ct, 'bl, 'cl') assertion ->
  ('at, 'al) Semantics.Prog.ti -> unit
