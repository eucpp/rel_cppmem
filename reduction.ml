
module type Context =
  sig
    (** Term type *)
    type t
    
    (** Context type *)
    type c

    (** State type *) 
    type s
   
    type rresult = 
      | Skip
      | Conclusion of t * s

    type rule = (t * s -> rresult)
    
    val default_state : s    

    val split : t -> (c * t) list
    val plug : c * t -> t 
  end

module Interpreter (C : Context) =
  struct
    type t = (string * C.rule) list

    exception Rule_already_registered of string

    let create rules = rules

    let register_rule name rule rules = 
      if List.mem_assoc name rules 
      then raise (Rule_already_registered name) 
      else (name, rule)::rules

    let deregister_rule name rules = List.remove_assoc name rules

    let apply_rule (c, t) s rule =
      match (rule (t, s)) with 
        | C.Conclusion (t', s') -> [(c, t', s')]
        | C.Skip                -> []

    let apply_rules redex s rules =
         List.map (fun (_, rule) -> apply_rule redex s rule) rules
      |> List.concat

    let step rules (t, s) =
      let redexes = C.split t in
           List.map (fun redex -> apply_rules redex s rules) redexes
        |> List.concat
        |> List.map (fun (c', t', s') -> C.plug (c', t'), s')
      
    let rec space' rules cfgs = 
      let next =
           List.map (step rules) cfgs 
        |> List.concat
      in
        if next = cfgs
        then cfgs
        else space' rules next
    
    let space rules cfg = space' rules [cfg]
       
  end
