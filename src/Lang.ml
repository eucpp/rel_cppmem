open MiniKanren

module type ATerm =
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

module type AContext =
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
    val prj : lt -> c
    val show : c -> string

    val eq : c -> c -> bool

    (** [reducibleo t b] says whether term t could be reduced *)
    val reducibleo : lt -> bool logic -> goal

    (** [splito t c rdx] splits the term [t] into context [c] and redex [rdx] *)
    val splito :  lt ->  lc ->  lt -> goal
  end

module type AState =
  sig
    type t

    type lt'

    type lt = lt' logic

    val inj : t -> lt
    val prj : lt -> t
    val show : t -> string
    val eq : t -> t -> bool
  end

  type loc = string
  type tstmp = int

  type mem_order = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

  let string_of_loc = fun x -> x
  let string_of_tstmp = string_of_int

  let string_of_mo = function
    | SC      -> "sc"
    | ACQ     -> "acq"
    | REL     -> "rel"
    | ACQ_REL -> "relAcq"
    | CON     -> "con"
    | RLX     -> "rlx"
    | NA      -> "na"

  let mo_of_string str =
    let binding = [("sc", SC);
                   ("acq", ACQ);
                   ("rel", REL);
                   ("relAcq", ACQ_REL);
                   ("con", CON);
                   ("rlx", RLX);
                   ("na", NA)] in
      List.assoc str binding

module Path =
  struct
    type 'a at = N | L of 'a | R of 'a

    type t  = t  at
    type lt = lt at MiniKanren.logic

    let (!) = (!!)

    let rec inj = function
    | N -> !N
    | L p -> !(L (inj p))
    | R p -> !(R (inj p))


    let rec prj lp =
      let p = !?lp in
        match p with
        | N -> N
        | L lp' -> L (prj lp')
        | R lp' -> R (prj lp')
  end

module Term =
  struct
    include T5
    include FMap5

    @type ('int, 'string, 'mo, 'loc, 't) at =
    | Const    of 'int
    | Var      of 'string
    | Binop    of 'string * 't * 't
    | Asgn     of 't * 't
    | Pair     of 't * 't
    | If       of 't * 't * 't
    | Repeat   of 't
    | Read     of 'mo * 'loc
    | Write    of 'mo * 'loc * 't
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

    (* let rec inj t = !! (GT.gmap(at) (inj_nat) (!!) (!!) (!!) (inj) t) *)
    let inj t = inj @@ distrib t

    let rec prj lt = GT.gmap(at) (prj_nat) (!?) (!?) (!?) (prj) (!? lt)

    let show t =
      let kwd ff s = Format.fprintf ff "%s" s in
      let rec s ff = function
        | Const n                 -> Format.fprintf ff "@[%d@]" n
        | Var x                   -> Format.fprintf ff "@[%s@]" x
        | Binop (op, a, b)        -> Format.fprintf ff "@[%a %a %a@]" s a kwd op s b
        | Asgn (x, y)             -> Format.fprintf ff "@[<hv>%a := %a@]" s x s y
        | Pair (x, y)             -> Format.fprintf ff "@[(%a, %a)@]" s x s y
        | If (cond, t, f)         -> Format.fprintf ff "@[<v>if %a@;then %a@;else %a@]" s cond s t s f
        | Repeat t                -> Format.fprintf ff "@[repeat %a@]" s t
        | Read (mo, loc)          -> Format.fprintf ff "@[%s_%s@]" (string_of_loc loc) (string_of_mo mo)
        | Write (mo, loc, t)      -> Format.fprintf ff "@[%s_%s :=@;<1 4>%a@]" (string_of_loc loc) (string_of_mo mo) s t
        | Seq (t, t')             -> Format.fprintf ff "@[<v>%a;@;%a@]" s t s t'
        | Spw (t, t')             -> Format.fprintf ff "@[<v>spw {{{@;<1 4>%a@;|||@;<1 4>%a@;}}}@]" s t s t'
        | Par (t, t')             -> Format.fprintf ff "@[<v>par {{{@;<1 4>%a@;<1 4>|||@;<1 4>%a@;}}}@]" s t s t'
        | Skip                    -> Format.fprintf ff "@[skip@]"
        | Stuck                   -> Format.fprintf ff "@[stuck@]"
      in
      s Format.str_formatter t;
      Format.flush_str_formatter ()

    let rec eq t t' = GT.eq(at) (GT.eq(GT.int)) (GT.eq(GT.string)) (=) (=) (eq) t t'
  end

module Context =
  struct
    include T6
    include FMap6

    type t   = Term.t
    type lt' = Term.lt'
    type lt  = Term.lt

    @type ('expr, 'string, 'mo, 'loc, 't, 'c) ac =
    | Hole
    | BinopL    of 'string * 'c * 't
    | BinopR    of 'string * 't * 'c
    | PairL     of 'c * 't
    | PairR     of 't * 'c
    | AsgnC     of 't * 'c
    | WriteC    of 'mo * 'loc * 'c
    | IfC       of 'c * 't * 't
    | SeqC      of 'c * 't
    | ParL      of 'c * 't
    | ParR      of 't * 'c
    with gmap, eq, show

    type c   = (int, string, mem_order, loc, Term.t, c) ac
    type lc' = (Nat.logic, string logic, mem_order logic, loc logic, Term.lt, lc' logic) ac
    type lc  = lc' logic

    (* let rec inj c = !! (GT.gmap(ac) (inj_nat) (!!) (!!) (!!) (Term.inj) (inj) c) *)
    let inj t = inj @@ distrib t

    let rec prj lc = GT.gmap(ac) (prj_nat) (!?) (!?) (!?) (Term.prj) (prj) (!? lc)

    let rec show c = GT.show(ac) (GT.show(GT.int)) (GT.show(GT.string)) (string_of_mo) (string_of_loc) (Term.show) (show) c

    let rec eq c c' = GT.eq(ac) (GT.eq(GT.int)) (GT.eq(GT.string)) (=) (=) (Term.eq) (eq) c c'

    let (!) = MiniKanren.inj

    let rec reducibleo t b = Term.(conde [
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
      fresh (mo l t')
        (b === !true)
        (t === !(Write (mo, l, t')));
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
           (t === !(Pair (t1, t2)))
           (reducibleo t1 b1)
           (reducibleo t2 b2)
           (Bool.oro b1 b2 b)
      ]);

      ((b === !false) &&& (t === !Skip));
      ((b === !false) &&& (t === !Stuck));
    ])

    let rec splito t c rdx = Term.(
      (conde [
        fresh (op l r c' t')
          (t === !(Binop (op, l, r)))
          (conde [
            ((c === !Hole)                 &&& (rdx === t));
            ((c === !(BinopL (op, c', r))) &&& (reducibleo l !true)  &&& (rdx === t') &&& (splito l c' t'));
            ((c === !(BinopR (op, l, c'))) &&& (reducibleo l !false) &&& (rdx === t') &&& (splito r c' t'));
          ]);

        fresh (t1 t2 c' t')
          (t === !(Pair (t1, t2)))
          (conde [
            ((c === !Hole)             &&& (rdx === t ));
            ((c === !(PairL (c', t2))) &&& (reducibleo t1 !true)  &&& (rdx === t') &&& (splito t1 c' t'));
            ((c === !(PairR (t1, c'))) &&& (reducibleo t1 !false) &&& (rdx === t') &&& (splito t2 c' t'));
          ]);

        fresh (l r c' t')
          (t === !(Asgn (l, r)))
          (conde [
            ((c === !Hole)            &&& (rdx === t ));
            ((c === !(AsgnC (l, c'))) &&& (rdx === t') &&& (splito r c' t'));
          ]);

        fresh (mo loc e c' t')
          (t === !(Write (mo, loc, e)))
          (conde [
            ((c === !Hole)                   &&& (rdx === t ));
            ((c === !(WriteC (mo, loc, c'))) &&& (rdx === t') &&& (splito e c' t'));
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
            (* ((c === !Hole)            &&& (rdx === t )); *)
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

          fresh (t')
            (t === !(Repeat t'));

          fresh (mo l)
            (t === !(Read (mo, l)));

          fresh (mo1 mo2 l t1 t2)
            (t === !(Cas (mo1, mo2, l, t1, t2)));

          fresh (t1 t2)
            (t === !(Spw (t1, t2)));

          (t === !Skip);

          (t === !Stuck);
        ]);
      ]))

      let rec plugo t c rdx = Term.(
        (conde [
          fresh (op l r c' t')
            (t === !(Binop (op, l, r)))
            (conde [
              ((c === !Hole)                 &&& (rdx === t));
              ((c === !(BinopL (op, c', r))) &&& (rdx === t') &&& (plugo l c' t'));
              ((c === !(BinopR (op, l, c'))) &&& (rdx === t') &&& (plugo r c' t'));
            ]);

          fresh (t1 t2 c' t')
            (t === !(Pair (t1, t2)))
            (conde [
              ((c === !Hole)             &&& (rdx === t ));
              ((c === !(PairL (c', t2))) &&& (rdx === t') &&& (plugo t1 c' t'));
              ((c === !(PairR (t1, c'))) &&& (rdx === t') &&& (plugo t2 c' t'));
            ]);

          fresh (l r c' t')
            (t === !(Asgn (l, r)))
            (conde [
              ((c === !Hole)            &&& (rdx === t ));
              ((c === !(AsgnC (l, c'))) &&& (rdx === t') &&& (plugo r c' t'));
            ]);

          fresh (mo loc e c' t')
            (t === !(Write (mo, loc, e)))
            (conde [
              ((c === !Hole)                   &&& (rdx === t ));
              ((c === !(WriteC (mo, loc, c'))) &&& (rdx === t') &&& (plugo e c' t'));
            ]);

          fresh (cond btrue bfalse c' t')
            (t === !(If (cond, btrue, bfalse)))
            (conde [
              ((c === !Hole)                      &&& (rdx === t ));
              ((c === !(IfC (c', btrue, bfalse))) &&& (rdx === t') &&& (plugo cond c' t'))
            ]);

          fresh (t1 t2 c' t')
            (t === !(Seq (t1, t2)))
            (conde [
              ((c === !Hole)            &&& (rdx === t ));
              ((c === !(SeqC (c', t2))) &&& conde [
                (rdx === t') &&& (plugo t1 c' t');
                (rdx === !Skip)  &&& (t === t2);
                (rdx === !Stuck) &&& (t === !Stuck);
              ]);
            ]);

          fresh (t1 t2 c' t')
            (t === !(Par (t1, t2)))
            (conde [
               ((c === !Hole)            &&& (rdx === t ));
               ((c === !(ParL (c', t2))) &&& (rdx === t') &&& (plugo t1 c' t'));
               ((c === !(ParR (t1, c'))) &&& (rdx === t') &&& (plugo t2 c' t'));
            ]);

          ((c === !Hole) &&& (rdx === t) &&& conde [
            fresh (n)
              (t === !(Const n));

            fresh (x)
              (t === !(Var x));

            fresh (t')
              (t === !(Repeat t'));

            fresh (mo l)
              (t === !(Read (mo, l)));

            fresh (mo1 mo2 l t1 t2)
              (t === !(Cas (mo1, mo2, l, t1, t2)));

            fresh (t1 t2)
              (t === !(Spw (t1, t2)));

            (t === !Skip);

            (t === !Stuck);
          ]);
        ]))

      let rec patho c path = Term.(
        fresh (op mo loc t1 t2 t3 t' c' path')
          (conde [
            (c === !Hole)                   &&& (path === !Path.N);
            (c === !(BinopL (op, c', t1)))  &&& (patho c' path);
            (c === !(BinopR (op, t1, c')))  &&& (patho c' path);
            (c === !(PairL (c', t2)))       &&& (patho c' path);
            (c === !(PairR (t1, c')))       &&& (patho c' path);
            (c === !(AsgnC (t1, c')))       &&& (patho c' path);
            (c === !(WriteC (mo, loc, c'))) &&& (patho c' path);
            (c === !(IfC (c', t2, t3)))     &&& (patho c' path);
            (c === !(SeqC (c', t1)))        &&& (patho c' path);
            (c === !(ParL (c', t1)))        &&& (path === !(Path.L path')) &&& (patho c' path');
            (c === !(ParR (t1, c')))        &&& (path === !(Path.R path')) &&& (patho c' path');
          ])
      )
  end
