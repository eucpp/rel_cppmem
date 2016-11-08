

module type ReductionContext =
  sig
    (** Term type *)
    type t
    
    (** Context type *)
    type c

    (** State type *) 
    type s
   
    val get_contexts : t -> c * t list
    val apply_context : c * t -> t list 
  end

module ReductionSemantic (C : ReductionContext) :
  sig
    (** Type of the semantic *) 
    type t

    module C : ReductionContext

    type rule_result = 
      (** rule is not applicable to given term *)
      | Skip
      (** result of successful application *)
      | Conclusion of C.t * C.s
                              
    val register_rule : string -> (C.t -> C.s -> rule_result) -> unit
    val deregister_rule : string -> unit

    (** Performs single step in given semantic *)
    val step : C.t * C.s -> C.t * C.c list

    (** Returns states that are reachable from initial state *)
    val space : C.t * C.s -> C.t * C.c list 

  end
