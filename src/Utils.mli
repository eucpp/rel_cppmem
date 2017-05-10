
val pprint_logic : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a MiniKanren.logic -> unit

val pprint_llist : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a MiniKanren.List.logic -> unit

val pprint_nat : Format.formatter -> MiniKanren.Nat.logic -> unit
val pprint_string : Format.formatter -> string MiniKanren.logic -> unit

val zip3 : 'a MiniKanren.Stream.t -> 'b MiniKanren.Stream.t -> 'c MiniKanren.Stream.t -> ('a * 'b * 'c) MiniKanren.Stream.t

module Option :
  sig
    exception No_value

    val is_some : 'a option -> bool
    val is_none : 'a option -> bool
    val get : 'a option -> 'a
  end
