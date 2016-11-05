type mem_order = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

type loc = Loc of string

type binop = Add | Sub | Mult | Div
                    
type stmt =
  | Const    of int
  | Var      of string
  | Binop    of binop * stmt * stmt
  
  | Read     of mem_order * loc
  | Write    of mem_order * loc * stmt
  | Cas      of mem_order * mem_order * loc * stmt * stmt

  | Asgn     of stmt * stmt

  | Seq      of stmt * stmt
  | Par      of stmt * stmt

type context =
  | Hole

  | BinopL   of binop * context * stmt
  | BinopR   of binop * stmt * context

  | AsgnL    of context * stmt
  | AsgnR    of stmt * context

  | Seq      of context * stmt
  | ParL     of context * stmt
  | ParR     of stmt * context                            

type path = N | L of path | R of path

let rec getPath : context -> path = function
  | Hole -> N

  | BinopL (_, c, _)
  | BinopR (_, _, c)
  | AsgnL  (c, _)
  | AsgnR  (_, c)
  | Seq    (c, _)
    -> getPath c

  | ParL (c, _) -> L (getPath c)
  | ParR (_, c) -> R (getPath c)                     

let isValue : stmt -> bool = function
  | Const _
  | Var   _
      -> true
  | _ -> false
           
let rec getContexts : stmt -> (context * stmt) list = function
  | Par  (l, r) as t ->
     let lcontexts = List.map (fun (c, t) -> (ParL (c, r), t)) (getContexts l) in
     let rcontexts = List.map (fun (c, t) -> (ParR (l, c), t)) (getContexts r) in
     (match isValue l, isValue r with
        | false, false -> lcontexts @ rcontexts
        | _    , false -> rcontexts
        | false, _     -> lcontexts
        | _            -> [Hole, t]
     )
     
  | t -> [Hole, t]
                                            
let binopToString = function
  | Add  -> "+"
  | Sub  -> "-"
  | Mult -> "*"
  | Div  -> "/"

let rec stmtToString = function
  | Const n            -> string_of_int n
  | Var   x            -> x
  | Binop (op, s1, s2) -> "(" ^ (stmtToString s1) ^ (binopToString op) ^ (stmtToString s2) ^ ")"

  (*                                                                                               
  | Read     of mem_order * loc
  | Write    of mem_order * loc * stmt
  | Cas      of mem_order * mem_order * loc * stmt * stmt

  | Asgn     of stmt * stmt
  *)

  | Seq (s1, s2)       -> (stmtToString s1) ^ ";\n" ^ (stmtToString s2) 
  | Par (s1, s2)       -> "{{{\n" ^ (stmtToString s1) ^ "\n|||\n" ^ (stmtToString s2) ^ "\n}}}\n"
              
let rec contextToString = function
  | Hole               -> "Hole"

  | BinopL (op, c, t)  -> (binopToString op) ^ " [" ^ (contextToString c) ^ "] " ^ (stmtToString t)
  | BinopR (op, t, c)  -> (binopToString op) ^ " " ^  (stmtToString t)    ^ " [" ^ (contextToString c) ^ "]"

  | AsgnL  (c, t)      -> "[" ^ (contextToString c) ^ "] " ^ (stmtToString t)    
  | AsgnR  (t, c)      ->       (stmtToString t)    ^ " [" ^ (contextToString c) ^ "]"

  | Seq    (c, t)      -> "[" ^ (contextToString c) ^ "] " ^ (stmtToString t)
  | ParL   (c, t)      -> "[" ^ (contextToString c) ^ "] " ^ (stmtToString t)  
  | ParR   (t, c)      ->       (stmtToString t)    ^ " [" ^ (contextToString c) ^ "]"                            

let rec contextsToString = function
  | []          -> ""
  | (c, t)::cs  -> (contextToString c) ^ " in " ^ (stmtToString t) ^ "\n" ^ (contextsToString cs) 


let t = Par (Binop (Mult, Const 2, Const 42), Binop (Add, Const 0, Const 2));;

let ctxs = getContexts t;;            
                                                                           
let _ =
  print_string (contextsToString ctxs);
  print_newline;
