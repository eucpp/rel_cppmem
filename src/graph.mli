
(** Type of graph *)
type ('v, 'e) t

val create : unit -> ('v, 'e) t

val connect : ('v, 'e) t -> 'v -> 'v -> e' -> unit
val disconnect : ('v, 'e) t -> 'v -> 'v -> t

val outdegree : ('v, 'e) t -> 'v -> int 

val sinks : ('v, 'e) t -> 'v list

val iter_vertices :  ('v, 'e) t -> ('v -> unit) -> 'unit
val iter_neighbors : ('v, 'e) t -> 'v -> ('v * 'e -> unit) -> unit  

