open MiniKanren

val excl_answ : 'a Stream.t -> 'a

val inj_assoc : ('a -> 'c logic) -> ('b -> 'd logic) -> ('a * 'b) list -> ('c logic * 'd logic) logic MiniKanren.List.logic
val prj_assoc : ('c logic -> 'a) -> ('d logic -> 'b) -> ('c logic * 'd logic) logic MiniKanren.List.logic -> ('a * 'b) list

val show_assoc : ('a -> string) -> ('b -> string) -> ('a * 'b) list -> string
val eq_assoc : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a * 'b) list -> ('a * 'b) list -> bool

val assoco : 'a logic -> ('a logic  * 'b logic) logic MiniKanren.List.logic -> 'b logic -> MiniKanren.goal
val remove_assoco : 'a logic -> ('a logic * 'b logic) logic MiniKanren.List.logic  -> ('a logic * 'b logic) logic MiniKanren.List.logic -> goal 

val update_assoco : 'a logic -> 'b logic -> ('a logic * 'b logic) logic MiniKanren.List.logic -> ('a logic * 'b logic) logic MiniKanren.List.logic -> goal

module Option : 
  sig
    exception No_value
    
    val is_some : 'a option -> bool
    val is_none : 'a option -> bool
    val get : 'a option -> 'a    
  end

