
module type Context =
  sig
    (** Term type *)
    type t
    
    (** Context type *)
    type c

    (** State type *) 
    type s
   
    (** Type of rules *) 
    type rule

    val split : t -> (c * t) list
    val merge : c * t -> t 
  end

module Interpreter (C : Context) =
  struct
    type t = Rules of (string * C.rule) list

    exception Rule_already_registered of string

    let create rules = Rules rules

    let register_rule (Rules rules) name rule = 
        if List.mem_assoc name rules 
        then raise (Rule_already_registered name) 
        else Rules ((name, rule)::rules)
    
    let deregister_rule (Rules rules) name = Rules (List.remove_assoc name rules)

    (** Performs single step in given semantic *)
    let step (Rules rules) (t, c) = 
        
    (** Returns states that are reachable from initial state *)
    val space : C.t * C.s -> C.t * C.c list 

  end
