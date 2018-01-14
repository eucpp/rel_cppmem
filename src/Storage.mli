
type ('at, 'bt) tt

type ('al, 'bl) tl = ('al, 'bl) inner MiniKanren.logic
  and ('al, 'bl) inner

type ('at, 'bt, 'al, 'bl) ti = (('at, 'bt) tt, ('al, 'bl) tl) MiniKanren.injected

type ('at, 'al) key = ('at, 'al) MiniKanren.injected
type ('bt, 'bl) value = ('bt, 'bl) MiniKanren.injected

val empty : unit -> ('at, 'bt, 'al, 'bl) ti

val allocate : ('bt, 'bl) value -> ('at, 'al) key list -> ('at, 'bt, 'al, 'bl) ti

val from_assoc : (('at, 'al) key * ('bt, 'bl) value) list -> ('at, 'bt, 'al, 'bl) ti

val reify :
  (MiniKanren.helper -> ('at, 'al) MiniKanren.injected -> 'al) ->
  (MiniKanren.helper -> ('bt, 'bl) MiniKanren.injected -> 'bl) ->
  MiniKanren.helper -> ('at, 'bt, 'al, 'bl) ti -> ('al, 'bl) tl

val pprint : (Format.formatter -> 'al * 'bl -> unit) -> Format.formatter -> ('al, 'bl) tl -> unit

val geto : ('at, 'bt, 'al, 'bl) ti ->                            ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal
val seto : ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal

val removeo :
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key -> MiniKanren.goal

val membero :
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal

val extendo :
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti ->
  ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal

val updateo :
  (('bt, 'bl) value -> ('bt, 'bl) value -> MiniKanren.goal) ->
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key -> MiniKanren.goal

val mapo :
  (('at, 'al) key -> ('bt, 'bl) value -> ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal) ->
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> MiniKanren.goal

val map2o :
  (('at, 'al) key -> ('bt, 'bl) value -> ('at, 'al) key -> ('bt, 'bl) value -> ('at, 'al) key -> ('bt, 'bl) value -> MiniKanren.goal) ->
  ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> ('at, 'bt, 'al, 'bl) ti -> MiniKanren.goal

val shapeo : ('at, 'bt, 'al, 'bl) ti -> ('at, 'al) key list -> MiniKanren.goal

val constro :
  ('at, 'bt, 'al, 'bl) ti ->
  (('at, 'al) key * (('bt, 'bl) value -> MiniKanren.goal)) list ->
  MiniKanren.goal
