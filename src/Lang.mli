module type Term =
  sig
    (** Term type *)
    type t
    
    type lt'

    (** Injection of term into logic domain *)
    type lt = lt' MiniKanren.logic

    val inj : t -> lt
    val prj : lt -> t
    val show : t -> string
    (* val parse : string -> t *)
    val eq : t -> t -> bool
  end

module type Context =
  sig
    (** Term type *)
    type t
           
    type lt'

    (** Injection of term into MiniKanren.logic domain *)
    type lt = lt' MiniKanren.logic
    
    (** Context type *)
    type c

    type lc'

    (** Injection of context into logic domain *)
    type lc = lc' MiniKanren.logic

    val inj : c -> lc
    val prj : lc -> c
    val show : c -> string
    val eq : c -> c -> bool
                     
    (** [reducibleo t b] says whether term t could be reduced *)
    val reducibleo : lt -> bool MiniKanren.logic -> MiniKanren.goal 

    (** [splito t c rdx] splits the term [t] into context [c] and redex [rdx] *)
    val splito :  lt ->  lc ->  lt -> MiniKanren.goal

  end

module type State = 
  sig 
    type t
    
    type lt'

    type lt = lt' MiniKanren.logic 

    val inj : t -> lt
    val prj : lt -> t
    val show : t -> string
    val eq : t -> t -> bool
  end

module ExprTerm :
  sig
    @type ('int, 'string, 't) at =
    | Const of 'int
    | Var   of 'string
    | Binop of 'string * 't * 't
    | Stuck
    with gmap, eq, show 

    type t   = (int, string, t) at
    type lt' = (MiniKanren.Nat.logic, string MiniKanren.logic, lt' MiniKanren.logic) at
    type lt  = lt' MiniKanren.logic

    val inj : t -> lt
    val prj : lt -> t
    val show : t -> string
    (* val parse : string -> t *)
    val eq : t -> t -> bool
  end

module ExprContext :
  sig
    type t   = ExprTerm.t
    type lt' = ExprTerm.lt'
    type lt  = ExprTerm.lt

    @type ('int, 'string, 't, 'c) ac = 
    | Hole
    | BinopL of 'string * 'c * 't
    | BinopR of 'string * 't * 'c
    with gmap, eq, show

    type c   = (int, string, t, c) ac
    type lc' = (MiniKanren.Nat.logic, string MiniKanren.logic, lt, lc' MiniKanren.logic) ac
    type lc  = lc' MiniKanren.logic

    val inj : c -> lc
    val prj : lc -> c
    val show : c -> string
    val eq : c -> c -> bool

    val reducibleo : lt -> bool MiniKanren.logic -> MiniKanren.goal

    val splito : lt -> lc -> lt -> MiniKanren.goal
  end 

module StmtTerm : 
  sig
    @type ('expr, 'string, 'mo, 'loc, 't) at =
    | AExpr    of 'expr
    | Asgn     of 't * 't
    | Pair     of 'expr * 'expr
    | If       of 't * 't * 't
    | Repeat   of 't
    | Read     of 'mo * 'loc
    | Write    of 'mo * 'loc * 'expr
    | Cas      of 'mo * 'mo * 'loc * 'expr * 'expr
    | Seq      of 't * 't
    | Spw      of 't * 't
    | Par      of 't * 't
    | Skip
    | Stuck
    with gmap, eq, show

    type t   = (ExprTerm.t, string, Memory.mem_order, Memory.loc, t) at
    type lt' = (ExprTerm.lt, string MiniKanren.logic, Memory.mem_order MiniKanren.logic, Memory.loc MiniKanren.logic, lt' MiniKanren.logic) at
    type lt  = lt' MiniKanren.logic

    val inj : t -> lt
    val prj : lt -> t
    val show : t -> string
    (* val parse : string -> t *)
    val eq : t -> t -> bool
  end

module StmtContext : 
  sig
    type t   = StmtTerm.t
    type lt' = StmtTerm.lt'
    type lt  = StmtTerm.lt

    @type ('expr, 'string, 'mo, 'loc, 't, 'c) ac =
    | Hole
    | AsgnC     of 't * 'c
    | IfC       of 'c * 't * 't
    | SeqC      of 'c * 't
    | ParL      of 'c * 't
    | ParR      of 't * 'c
    with gmap, eq, show

    type c   = (ExprTerm.t, string, Memory.mem_order, Memory.loc, StmtTerm.t, c) ac
    type lc' = (ExprTerm.lt, string MiniKanren.logic, Memory.mem_order MiniKanren.logic, Memory.loc MiniKanren.logic, StmtTerm.lt, lc' MiniKanren.logic) ac
    type lc  = lc' MiniKanren.logic

    val inj : c -> lc

    val prj : lc -> c

    val show : c -> string

    val eq : c -> c -> bool

    val reducibleo : lt -> MiniKanren.Bool.logic -> MiniKanren.goal

    val splito : lt -> lc -> lt -> MiniKanren.goal

    val patho : lc -> Memory.Path.lt -> MiniKanren.goal
  end
