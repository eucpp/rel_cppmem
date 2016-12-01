module type Term = 
  sig
    (** Term type *)
    type t
    
    (** Injection of term into logic domain *)
    type lt

    val inj : t -> lt
    val prj : lt -> t
    val show : t -> string 
    val eq : t -> t -> bool   
  end

module type Context = 
  sig 
    (** Term type *)
    type t
    
    (** Injection of term into MiniKanren.logic domain *)
    type lt
    
    (** Context type *)
    type c

    (** Injection of context into MiniKanren.logic domain *)
    type lc

    val inj : c -> lc
    val prj : lc -> c
    val show : c -> string 
    val eq : c -> c -> bool

    val inj_term : t -> lt
    val prj_term : lt -> t

    val inj_ctx : c -> lc
    val prj_ctx : lc -> c

    (** [splito t c rdx] splits the term [t] into context [c] and redex [rdx] *) 
    val splito : lt -> lc -> lt -> MiniKanren.goal
  end

module Reducer (C : Context) :
  sig
    type t = C.t
    type c = C.c

    (** Non-relational wrapper for splito *)
    val split : t -> (c * t) MiniKanren.Stream.t

    (** Non-relational wrapper for plugging term into context *)
    val plug : (c * t) -> t  
  end

module ExprTerm :
  sig
   (** We temporarily export type definition until we implement parsing from string *) 
   @type ('int, 'string, 't) at =
    | Const of 'int
    | Var   of 'string
    | Binop of 'string * 't * 't
    | Stuck
    with gmap, eq, show 

    type t  = (int, string, t) at
    type lt = (int MiniKanren.logic, string MiniKanren.logic, lt) at MiniKanren.logic

    val inj : t -> lt
    val prj : lt -> t
    val show : t -> string 
    val eq : t -> t -> bool
  end  

module ExprContext : 
  sig 
    type t  = ExprTerm.t
    type lt = ExprTerm.lt

    @type ('int, 'string, 't, 'c) ac = 
    | Hole
    | BinopL of 'string * 'c * 't
    | BinopR of 'string * 't * 'c
    with gmap, eq, show

    type c  = (int, string, t, c) ac
    type lc = (int MiniKanren.logic, string MiniKanren.logic, lt, lc) ac MiniKanren.logic

    val inj : c -> lc
    val prj : lc -> c
    val show : c -> string 
    val eq : c -> c -> bool

    val inj_term : t -> lt
    val prj_term : lt -> t

    val inj_ctx : c -> lc
    val prj_ctx : lc -> c

    (** [splito t c rdx] splits the term [t] into context [c] and redex [rdx] *) 
    val splito : lt -> lc -> lt -> MiniKanren.goal
  end
