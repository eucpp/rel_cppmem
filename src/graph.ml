
type ('v, 'e) t = ('v, ('v * 'e) list) Hashtbl.t

exception Duplicate_edge

let init_size = 10

let create = Hashtbl.create init_size

let connect_exn g v u e = 
  let adjlist = try Hashtbl.find g v with
                  | Not_found -> []
  in
    if List.exists (fun (u', e') -> u' = u) adjlist
    then raise Duplicate_edge
    else Hashtbl.replace g v ((u, e)::adjlist)

let connect g v u e = try connect_exn g v u e with
                        | Duplicate_edge -> () 

let outdegree g v = 
  let adjlist = Hashtbl.find g v in
    List.length adjlist

let disconnect g v u =
  let adjlist  = Hashtbl.find g v in
  let adjlist' = List.remove_assoc u adjlist in
    Hashtbl.replace g v adjlist'  

let iter_vertices g f = Hashtbl.iter (fun v adjlist -> f v) g

let iter_neighbors g v f = 
  let adjlist = Hashtbl.find g v in
    List.iter f adjlist  

let sinks g = 
  let ss = ref [] in
  let append u = ss := u::!ss in 
    iter_vertices g (fun v -> if (outdegree g v) = 0 then append v else ());
    ss
