module Make
  (T : Lang.Term) 
  (C : Lang.Context with type t = T.t with type 'a lt = 'a MiniKanren.logic)
  (S : Lang.State with type 'a lt = 'a MiniKanren.logic) :
  sig
    type t

    type ('a, 'b, 'c) rule = ('a C.lc -> 'b T.lt -> 'c S.lt -> 'a C.lc -> 'b T.lt -> 'c S.lt -> MiniKanren.goal)

    val empty : t
    
    val make : (string * (_,_,_) rule) list -> t

    val register   : string * _ rule -> t -> t
    val deregister : string -> t -> t 

    val stepo : t -> _ T.lt -> _ S.lt -> _ T.lt -> _ S.lt -> MiniKanren.goal

    (** Non-relational wrapper for split *)
    val split : T.t -> (C.c * T.t) MiniKanren.Stream.t

    (** Non-relational wrapper for plugging term into context *)
    val plug : (C.c * T.t) -> T.t
  end
