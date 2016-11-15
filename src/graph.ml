
type ('v, 'e) t = ('v, ('v * 'e) list) Hashtbl.t

exception Duplicate_vertex
exception Duplicate_edge

let init_size = 10

let create = fun _ -> Hashtbl.create init_size

let vertex_exists g v =
  try 
    Hashtbl.find g v;
    true
  with
    | Not_found -> false 
    
let add_vertex g v =
  if vertex_exists g v 
  then raise Duplicate_vertex
  else Hashtbl.add g v []

let connect g v u e = 
  let adjlist = try Hashtbl.find g v with
                  | Not_found -> []
  in
    add_vertex g u;
    if List.exists (fun (u', e') -> u' = u) adjlist
    then raise Duplicate_edge
    else Hashtbl.replace g v ((u, e)::adjlist)

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
    !ss
