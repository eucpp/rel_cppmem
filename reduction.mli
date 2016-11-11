

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
      | Skip
      | Conclusion of t * s

    (** Type of rules *) 
    type rule = (t * s -> rresult)

    val split : t -> (c * t) list
    val plug : c * t -> t 
  end

module Interpreter (C : Context) :
  sig
    (** Type of the interpreter *) 
    type t

    exception Rule_already_registered of string

    val create : string * C.rule list -> t

    val register_rule : t -> string -> C.rule -> t
    val deregister_rule : t -> string -> t

    (** Performs single step in given semantic *)
    val step : t -> C.t * C.s -> C.t * C.s list

    (** Returns states that are reachable from initial state *)
    val space : t -> C.t * C.s -> C.t * C.s list 

  end
