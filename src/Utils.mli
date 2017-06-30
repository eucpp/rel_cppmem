
val pprint_logic : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a MiniKanren.logic -> unit

val pprint_llist : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a MiniKanrenStd.List.logic -> unit

val pprint_nat : Format.formatter -> MiniKanrenStd.Nat.logic -> unit
val pprint_string : Format.formatter -> string MiniKanren.logic -> unit

val zip3 : 'a MiniKanren.Stream.t ->
           'b MiniKanren.Stream.t ->
           'c MiniKanren.Stream.t ->
           ('a * 'b * 'c) MiniKanren.Stream.t

val zip4 : 'a MiniKanren.Stream.t ->
           'b MiniKanren.Stream.t ->
           'c MiniKanren.Stream.t ->
           'd MiniKanren.Stream.t ->
           ('a * 'b * 'c * 'd) MiniKanren.Stream.t

val zip5 : 'a MiniKanren.Stream.t ->
           'b MiniKanren.Stream.t ->
           'c MiniKanren.Stream.t ->
           'd MiniKanren.Stream.t ->
           'e MiniKanren.Stream.t ->
           ('a * 'b * 'c * 'd * 'e) MiniKanren.Stream.t

val zip6 : 'a MiniKanren.Stream.t ->
           'b MiniKanren.Stream.t ->
           'c MiniKanren.Stream.t ->
           'd MiniKanren.Stream.t ->
           'e MiniKanren.Stream.t ->
           'f MiniKanren.Stream.t ->
           ('a * 'b * 'c * 'd * 'e * 'f) MiniKanren.Stream.t


module Option :
  sig
    exception No_value

    val is_some : 'a option -> bool
    val is_none : 'a option -> bool
    val get : 'a option -> 'a
  end
