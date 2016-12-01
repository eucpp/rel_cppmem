open GT
open MiniKanren
open Memory

module ExprTerm = 
  struct
    @type ('int, 'string, 't) at =
    | Const of 'int
    | Var   of 'string
    | Binop of 'string * 't * 't
    | Stuck
    with gmap, eq, show 

    type t  = (int, string, t) at
    type lt = (int logic, string logic, lt) at logic

    let rec inj t = !! (gmap(at) (!!) (!!) (inj) t)

    let rec prj lt = gmap(at) (!?) (!?) (prj) (!? lt)

    let rec show t = GT.show(at) (GT.show(int)) (GT.show(string)) (show) t

    let rec eq t t' = GT.eq(at) (GT.eq(int)) (GT.eq(string)) (eq) t t'
  end

module ExprContext =
  struct
    type t  = ExprTerm.t
    type lt = ExprTerm.lt

    @type ('int, 'string, 't, 'c) ac = 
    | Hole
    | BinopL of 'string * 'c * 't
    | BinopR of 'string * 't * 'c
    with gmap, eq, show

    

    type c  = (int, string, t, c) ac
    type lc = (int logic, string logic, lt, lc) ac logic

    let rec inj c = !! (gmap(ac) (!!) (!!) (ExprTerm.inj) (inj) c)

    let rec prj lc = gmap(ac) (!?) (!?) (ExprTerm.prj) (prj) (!? lc)

    let rec show c = GT.show(ac) (GT.show(int)) (GT.show(string)) (ExprTerm.show) (show) c 

    let rec eq c c' = GT.eq(ac) (GT.eq(int)) (GT.eq(string)) (ExprTerm.eq) (eq) c c'

    let (!) = MiniKanren.inj
    let (?) = MiniKanren.prj

    let reducibleo t b = ExprTerm.(conde [
      fresh (n)      (b === !false) (t === !(Const n));
      fresh (x)      (b === !true)  (t === !(Var x));
      fresh (op l r) (b === !true)  (t === !(Binop (op, l, r)));
    ])

    let rec splito t c rdx = ExprTerm.(conde [
      fresh (op l r c' t')
         (t === !(Binop (op, l, r)))
         (conde [
           ((c === !(BinopL (op, c', r))) &&& (rdx === t') &&& (reducibleo l !true ) &&& (splito l c' t'));
           ((c === !(BinopR (op, l, c'))) &&& (rdx === t') &&& (reducibleo r !true ) &&& (splito r c' t'));
           ((c === !Hole)                 &&& (rdx === t)  &&& (reducibleo l !false) &&& (reducibleo r !false));
        ]);
      fresh (x)
        ((t === !(Var x)) &&& (c === !Hole) &&& (rdx === t));
      fresh (n)
        ((t === !(Const n)) &&& (c === !Hole) &&& (rdx === t));
    ])

    let split t = run qr (fun q  r  -> splito (ExprTerm.inj t) q r)
                         (fun qs rs -> Stream.zip (Stream.map prj qs) (Stream.map ExprTerm.prj rs))

    let plug (c, t) = run q (fun q  -> splito q (inj c) (ExprTerm.inj t))
                            (fun qs -> Stream.map ExprTerm.prj qs)
  end 
 
module StmtTerm = 
  struct
    @type ('expr, 'string, 'mo, 'loc, 't) at =
    | AExpr    of 'expr
    | Asgn     of 'string * 't
    | If       of 'expr * 't * 't
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

    type t  = (ExprTerm.t, string, mem_order, loc, t) at
    type lt = (ExprTerm.t logic, string logic, mem_order logic, loc logic, lt) at logic

    let rec inj t = !! (gmap(at) (ExprTerm.inj) (!!) (!!) (!!) (inj) t)

    let rec prj lt = gmap(at) (ExprTerm.prj) (!?) (!?) (!?) (prj) (!? lt)

    let rec show t = GT.show(at) (ExprTerm.show) (GT.show(string)) (string_of_mo) (string_of_loc) (show) t

    let rec eq t t' = GT.eq(at) (ExprTerm.eq) (GT.eq(string)) (=) (=) (eq) t t'
  end

module StmtContext = 
  struct
    type t  = StmtTerm.t
    type lt = StmtTerm.lt

    @type ('expr, 'string, 'mo, 'loc, 't, 'c) ac =
    | Hole
    | AsgnC     of 'string * 'c
    | SeqC      of 'c * 't
    | ParL      of 'c * 't
    | ParR      of 't * 'c
    with gmap, eq, show

    type c  = (ExprTerm.t, string, mem_order, loc, StmtTerm.t, c) ac
    type lc = (ExprTerm.lt, string logic, mem_order logic, loc logic, StmtTerm.lt, lc) ac

    let rec inj c = !! (gmap(ac) (ExprTerm.inj) (!!) (!!) (!!) (StmtTerm.inj) (inj) c)

    let rec prj lc = gmap(ac) (ExprTerm.prj) (!?) (!?) (!?) (StmtTerm.prj) (prj) (!? lc)

    let rec show c = GT.show(ac) (ExprTerm.show) (GT.show(string)) (string_of_mo) (string_of_loc) (StmtTerm.show) (show) c

    let rec eq c c' = GT.eq(ac) (ExprTerm.eq) (GT.eq(string)) (=) (=) (StmtTerm.eq) (eq) c c'

    let (!) = MiniKanren.inj
    let (?) = MiniKanren.prj

    let reducibleo t b = StmtTerm.(conde [
      fresh (e) 
        (b === !false)
        (t === !(AExpr e));
      fresh (x r)
        (b === !true) 
        (t === !(Asgn (x, t)));
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
                                     (* ]) *)
      
      ((b === !false) &&& (t === !Skip));
      ((b === !false) &&& (t === !Stuck));   
    ])

    let rec splito t c rdx = StmtTerm.( 
      (conde [
        fresh (x r c' t')
          (t === !(Asgn (x, r)))
          (conde [
            ((c === !Hole)            &&& (rdx === t ) &&& (reducibleo r !false));
            ((c === !(AsgnC (x, c'))) &&& (rdx === t') &&& (reducibleo r !true ) &&& (splito r c' t'));
          ]);
 
        fresh (t1 t2 c' t')
          (t === !(Seq (t1, t2)))
          (conde [
            ((c === !Hole)            &&& (rdx === t ) &&& (reducibleo t1 !false)); 
            ((c === !(SeqC (c', t2))) &&& (rdx === t') &&& (reducibleo t1 !true) &&& (splito t1 c' t'));
          ]);

        fresh (t1 t2 c' t')
          (t === !(Par (t1, t2)))
          (conde [
             ((c === !Hole)            &&& (rdx === t ) &&& (reducibleo t1 !false) &&& (reducibleo t2 !false));
             ((c === !(ParL (c', t2))) &&& (rdx === t') &&& (reducibleo t1 !true ) &&& (splito t1 c' t'));
             ((c === !(ParR (t1, c'))) &&& (rdx === t') &&& (reducibleo t2 !true ) &&& (splito t2 c' t'));
          ]);

        ((c === !Hole) &&& (rdx === t) &&& conde [
          fresh (e) 
            (t === !(AExpr e));

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
  end
