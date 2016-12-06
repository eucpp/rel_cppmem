module Make
  (T : Lang.Term) 
  (C : Lang.Context with type t = T.t with type lt' = T.lt')
  (S : Lang.State) :
  sig
    type t

    type rule = (C.lc -> T.lt -> S.lt -> C.lc -> T.lt -> S.lt -> MiniKanren.goal)

    val empty : t
    
    val make : (string * rule) list -> t

    val register   : string * rule -> t -> t
    val deregister : string -> t -> t 

    (** Relational single step in given semantics *) 
    val stepo : t -> T.lt -> S.lt -> T.lt -> S.lt (* -> bool MiniKanren.logic *) -> MiniKanren.goal

    (** Relational step* *)
    val spaceo : t -> T.lt -> S.lt -> T.lt -> S.lt -> MiniKanren.goal

    (** Non-relational wrapper for split *)
    val split : T.t -> (C.c * T.t) MiniKanren.Stream.t

    (** Non-relational wrapper for plugging term into context *)
    val plug : (C.c * T.t) -> T.t

    (** Non-relational wrapper for stepping *)
    val step : t -> T.t -> S.t -> (T.t * S.t) MiniKanren.Stream.t

    (** Non-relational wrapper for step* *)
    val space : t -> T.t -> S.t -> (T.t * S.t) MiniKanren.Stream.t
  end
