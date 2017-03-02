open MiniKanren

val zip3 : 'a Stream.t -> 'b Stream.t -> 'c Stream.t -> ('a * 'b * 'c) Stream.t

val excl_answ : 'a Stream.t -> 'a

val prj_pair : ('a -> 'b) -> ('c -> 'd) -> ('a * 'c) logic -> ('b * 'd)

val key_eqo     : 'a logic -> ('a logic * 'b logic) logic -> bool logic -> goal
val key_not_eqo : 'a logic -> ('a logic * 'b logic) logic -> bool logic -> goal

val inj_assoc : ('a -> 'c logic) -> ('b -> 'd logic) -> ('a * 'b) list -> ('c logic * 'd logic) logic MiniKanren.List.logic
val prj_assoc : ('c -> 'a) -> ('d -> 'b) -> ('c * 'd) logic MiniKanren.List.logic -> ('a * 'b) list

val show_assoc : ('a -> string) -> ('b -> string) -> ('a * 'b) list -> string
val eq_assoc : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a * 'b) list -> ('a * 'b) list -> bool

val assoco : 'a logic -> ('a logic  * 'b logic) logic MiniKanren.List.logic -> 'b logic -> MiniKanren.goal
val assoc_defaulto : 'a logic -> ('a logic  * 'b logic) logic MiniKanren.List.logic -> 'b logic -> 'b logic -> MiniKanren.goal
val remove_assoco : 'a logic -> ('a logic * 'b logic) logic MiniKanren.List.logic  -> ('a logic * 'b logic) logic MiniKanren.List.logic -> goal 

val update_assoco_k : 'a logic -> ('a logic -> 'b logic option logic -> 'b logic -> goal) ->
                      ('a logic * 'b logic) logic MiniKanren.List.logic -> ('a logic * 'b logic) logic MiniKanren.List.logic -> goal
                                                                                                                                             
val update_assoco : 'a logic -> 'b logic ->
                    ('a logic * 'b logic) logic MiniKanren.List.logic -> ('a logic * 'b logic) logic MiniKanren.List.logic -> goal

val maxo : Nat.logic -> Nat.logic -> Nat.logic -> goal 

module Option : 
  sig
    exception No_value
    
    val is_some : 'a option -> bool
    val is_none : 'a option -> bool
    val get : 'a option -> 'a    
  end

