open MiniKanren
open Memory

module type Term =
  sig
    (** Term type *)
    type t
    
    type lt'

    (** Injection of term into logic domain *)
    type lt = lt' logic

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
    type lt = lt' logic
    
    (** Context type *)
    type c

    type lc'

    (** Injection of context into logic domain *)
    type lc = lc' logic

    val inj : c -> lc
    val prj : lc -> c
    val show : c -> string

    val eq : c -> c -> bool
                     
    (** [reducibleo t b] says whether term t could be reduced *)
    val reducibleo : lt -> bool logic -> goal 

    (** [splito t c rdx] splits the term [t] into context [c] and redex [rdx] *)
    val splito :  lt ->  lc ->  lt -> goal
  end

module type State = 
  sig 
    type t
    
    type lt'

    type lt = lt' logic 

    val inj : t -> lt
    val prj : lt -> t
    val show : t -> string
    val eq : t -> t -> bool
  end

module StmtTerm = 
  struct
    @type ('int, 'string, 'mo, 'loc, 't) at =
    | Const    of 'int
    | Var      of 'string
    | Binop    of 'string * 't * 't
    | Asgn     of 't * 't
    | Pair     of 't * 't
    | If       of 't * 't * 't
    | Repeat   of 't
    | Read     of 'mo * 'loc
    | Write    of 'mo * 'loc * 'expr
    | Cas      of 'mo * 'mo * 'loc * 't * 't
    | Seq      of 't * 't
    | Spw      of 't * 't
    | Par      of 't * 't
    | Skip
    | Stuck
    with gmap, eq, show

    type t   = (int, string, mem_order, loc, t) at
    type lt' = (Nat.logic, string logic, mem_order logic, loc logic, lt' logic) at
    type lt  = lt' logic

    let rec inj t = !! (GT.gmap(at) (!!) (!!) (!!) (!!) (inj) t)

    let rec prj lt = GT.gmap(at) (!?) (!?) (!?) (?!) (prj) (!? lt)

    let rec show t = GT.show(at) (GT.show(GT.int)) (GT.show(GT.string)) (string_of_mo) (string_of_loc) (show) t

    let rec eq t t' = GT.eq(at) (GT.eq(GT.int)) (GT.eq(GT.string)) (=) (=) (eq) t t'
  end

module StmtContext = 
  struct
    type t   = StmtTerm.t
    type lt' = StmtTerm.lt'
    type lt  = StmtTerm.lt

    @type ('expr, 'string, 'mo, 'loc, 't, 'c) ac =
    | Hole
    | BinopL    of 'string * 'c * 't
    | BinopR    of 'string * 't * 'c
    | AsgnC     of 't * 'c
    | IfC       of 'c * 't * 't
    | SeqC      of 'c * 't
    | ParL      of 'c * 't
    | ParR      of 't * 'c
    with gmap, eq, show

    type c   = (int, string, mem_order, loc, StmtTerm.t, c) ac
    type lc' = (Nat.logic, string logic, mem_order logic, loc logic, StmtTerm.lt, lc' logic) ac
    type lc  = lc' logic

    let rec inj c = !! (GT.gmap(ac) (!!) (!!) (!!) (!!) (StmtTerm.inj) (inj) c)

    let rec prj lc = GT.gmap(ac) (!?) (!?) (!?) (!?) (StmtTerm.prj) (prj) (!? lc)

    let rec show c = GT.show(ac) (GT.show(GT.int)) (GT.show(GT.string)) (string_of_mo) (string_of_loc) (StmtTerm.show) (show) c

    let rec eq c c' = GT.eq(ac) (GT.eq(GT.int)) (GT.eq(GT.string)) (=) (=) (StmtTerm.eq) (eq) c c'

    let (!) = MiniKanren.inj

    let reducibleo t b = StmtTerm.(conde [
      fresh (n)      
        (b === !false) 
        (t === !(Const n));
      fresh (x)      
        (b === !true)  
        (t === !(Var x));
      fresh (op l r) 
        (b === !true)  
        (t === !(Binop (op, l, r)));
      fresh (l r)
        (b === !true) 
        (t === !(Asgn (l, r)));
      fresh (e t1 t2)
        (b === !true)
        (t === !(If (e, t1, t2)));
      fresh (t')
        (b === !true)
        (t === !(Repeat t'));
      fresh (mo l)
        (b === !true)
        (t === !(Read (mo, l)));
      fresh (mo l e)
        (b === !true) 
        (t === !(Write (mo, l, e)));
      fresh (mo1 mo2 l e1 e2)
        (b === !true) 
        (t === !(Cas (mo1, mo2, l, e1, e2)));
      fresh (t1 t2)
        (b === !true)
        (t === !(Seq (t1, t2)));
      fresh (t1 t2)
        (b === !true)
        (t === !(Spw (t1, t2)));
      fresh (t1 t2)
        (b === !true)
        (t === !(Par (t1, t2)));

      (conde [
         fresh (t1 t2 b1 b2)
           (t === !(Pair (e1, e2)))
           (reducibleo t1 b1)
           (reducibleo t2 b2)
           (Bool.oro b1 b2 b)
      ]);
                                          
      ((b === !false) &&& (t === !Skip));
      ((b === !false) &&& (t === !Stuck));   
    ])

    let rec splito t c rdx = StmtTerm.( 
      (conde [
        fresh (op l r c' t')
          (t === !(Binop (op, l, r)))
          (conde [
            ((c === !(BinopL (op, c', r))) &&& (rdx === t') &&& (splito l c' t'));
            ((c === !(BinopR (op, l, c'))) &&& (rdx === t') &&& (splito r c' t'));
            ((c === !Hole)                 &&& (rdx === t));
          ]);

        fresh (l r c' t')
          (t === !(Asgn (l, r)))
          (conde [
            ((c === !Hole)            &&& (rdx === t ));
            ((c === !(AsgnC (l, c'))) &&& (rdx === t') &&& (splito r c' t'));
          ]);

        fresh (cond btrue bfalse c' t')
          (t === !(If (cond, btrue, bfalse)))
          (conde [
            ((c === !Hole)                      &&& (rdx === t ));
            ((c === !(IfC (c', btrue, bfalse))) &&& (rdx === t') &&& (splito cond c' t'))
          ]);
 
        fresh (t1 t2 c' t')
          (t === !(Seq (t1, t2)))
          (conde [
            ((c === !Hole)            &&& (rdx === t )); 
            ((c === !(SeqC (c', t2))) &&& (rdx === t') &&& (splito t1 c' t'));
          ]);

        fresh (t1 t2 c' t')
          (t === !(Par (t1, t2)))
          (conde [
             ((c === !Hole)            &&& (rdx === t ));
             ((c === !(ParL (c', t2))) &&& (rdx === t') &&& (splito t1 c' t'));
             ((c === !(ParR (t1, c'))) &&& (rdx === t') &&& (splito t2 c' t'));
          ]);

        ((c === !Hole) &&& (rdx === t) &&& conde [
          fresh (n)
            (t === !(Const n));

          fresh (x)
            (t === !(Var x));

          fresh (e) 
            (t === !(AExpr e));

          fresh (e1 e2)
            (t === !(Pair (e1, e2)));

          fresh (e t1 t2) 
            (t === !(If (e, t1, t2)));

          fresh (t') 
            (t === !(Repeat t'));

          fresh (mo l)
            (t === !(Read (mo, l)));

          fresh (mo l e)
            (t === !(Write (mo, l, e)));

          fresh (mo1 mo2 l e1 e2)
            (t === !(Cas (mo1, mo2, l, e1, e2)));

          fresh (t1 t2)
            (t === !(Spw (t1, t2)));

          (t === !Skip);

          (t === !Stuck);     
        ]);
      ]))

      let rec patho c path = StmtTerm.(
        fresh (op t1 t2 t3 t' c' path')
          (conde [
            (c === !Hole)                  &&& (path === !Memory.Path.N);
            (c === !(BinopL (op, c', t1))) &&& (patho c' path);
            (c === !(BinopR (op, t1, c'))) &&& (patho c' path);
            (c === !(AsgnC (t1, c')))      &&& (patho c' path);
            (c === !(IfC (t1, t2, t3)))    &&& (patho c' path);
            (c === !(SeqC (c', t1)))       &&& (patho c' path);
            (c === !(ParL (c', t1)))       &&& (path === !(Memory.Path.L path')) &&& (patho c' path');            
            (c === !(ParR (t1, c')))       &&& (path === !(Memory.Path.R path')) &&& (patho c' path');          
          ])
      )
  end
