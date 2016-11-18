

module type Context =
  sig
    (** Term type *)
    type t
    
    (** Context type *)
    type c

    (** State type *) 
    type s
   
    (** Result of rule application *) 
    type rresult = 
      | Conclusion of c * t * s
      | Rewrite    of t * s
      | Skip

    (** Type of rules *) 
    type rule = (c * t * s -> rresult list)

    val default_state : s

    val split : t -> (c * t) list
    val plug : c * t -> t 
  end

module Interpreter (C : Context) :
  sig
    (** Type of the interpreter *) 
    type t

    type edge = {rule : string; from : C.t * C.s; }

    exception Rule_already_registered of string

    val create : (string * C.rule) list -> t

    val register_rule : string -> C.rule -> t -> t
    val deregister_rule : string -> t -> t

    (** Performs single step in given semantic *)
    val step : t -> C.t * C.s -> (C.t * C.s) list
    
    (** Returns graph of states that are reachable from initial state *)
    val graph : t -> C.t * C.s -> (C.t * C.s, edge) Graph.t    

    (** Returns terminal states that are reachable from initial state *)
    val space : t -> C.t * C.s -> (C.t * C.s) list 

  end
