
module type Context =
  sig
    type t
    
    type c

    type s
   
    type rresult = 
      | Skip
      | Conclusion of c * t * s

    type rule = (c * t * s -> rresult)
    
    val default_state : s    

    val split : t -> (c * t) list
    val plug : c * t -> t 
  end

module Interpreter (C : Context) =
  struct
    type t = (string * C.rule) list

    type edge = {rule : string; from : C.t * C.s; }

    exception Rule_already_registered of string

    let create rules = rules

    let register_rule name rule rules = 
      if List.mem_assoc name rules 
      then raise (Rule_already_registered name) 
      else (name, rule)::rules

    let deregister_rule name rules = List.remove_assoc name rules

    let remove_duplicates xs = 
      let insert_unique ys x = 
        if List.exists ((=) x) ys
        then ys
        else x::ys
      in
           List.fold_left insert_unique [] xs  
        |> List.rev
        
    let apply_rule (c, t) s (name, rule) =
      match (rule (c, t, s)) with 
        | C.Conclusion (c', t', s') -> [(name, c', t', s')]
        | C.Skip                    -> []

    let apply_rules ((c, t) as redex) s rules =
         List.map (fun rule -> apply_rule redex s rule) rules 
      |> List.concat
      |> remove_duplicates

    let step' rules ((t, s) as cfg) g = 
      let redexes = C.split t in
           List.map (fun redex -> apply_rules redex s rules) redexes
        |> List.concat
        |> List.map (fun (rule, c', t', s') -> 
             let res = C.plug (c', t'), s' in  
               try
                 Graph.connect g cfg res {rule = rule; from = cfg};
                 Some res
               with
                 | Graph.Duplicate_edge -> None   
           )
        |> List.filter Utils.Option.is_some
        |> List.map Utils.Option.get

    let step rules cfg = step' rules cfg (Graph.create ())
      
    let rec graph' rules waiting g =
      if Queue.is_empty waiting
      then ()
      else
        let p   = Queue.pop waiting in
        let ps  = step' rules p g in
          List.iter (fun x -> Queue.push x waiting) ps; 
          graph' rules waiting g
      
    let graph rules init = 
      let q = Queue.create () in
      let g = Graph.create () in
        Queue.push init q;
        Graph.add_vertex g init; 
        graph' rules q g;
        g 

    let space rules init = 
      let g = graph rules init in
        Graph.sinks g

  end
