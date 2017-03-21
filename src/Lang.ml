open MiniKanren

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
    type 'a t = N | L of 'a | R of 'a

    type tt = tt t
    type tl = tl t logic
    type ti = (tt, tl) injected

    let fmap = failwith "Not implemented"

    let rec inj path = fmap inj path
  end

module Fmap1 = Fmap1(Path)

let pathn   = inj @@ Fmap1.distrib @@ Path.N
let pathl p = inj @@ Fmap1.distrib @@ Path.L p
let pathr p = inj @@ Fmap1.distrib @@ Path.R p

module Term =
  struct
    type ('int, 'string, 'mo, 'loc, 't) t =
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

    type tt   = (int, string, mem_order, loc, tt) t
    type tl  = (MiniKanren.Nat.logic, string MiniKanren.logic, mem_order MiniKanren.logic, loc MiniKanren.logic, tl) t MiniKanren.logic
    type ti  = (tt, tl) MiniKanren.injected

    let fmap fint fstring fmo floc ft t = failwith "Not implemented"

    let pprint t =
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
  end

module Fmap5 = Fmap5(Term)

let const n             = inj @@ Fmap5.distrib @@ Term.Const n
let var x               = inj @@ Fmap5.distrib @@ Term.Var x
let binop op l r        = inj @@ Fmap5.distrib @@ Term.Binop (op, l, r)
let asgn l r            = inj @@ Fmap5.distrib @@ Term.Asgn (l, r)
let pair l r            = inj @@ Fmap5.distrib @@ Term.Pair (l, r)
let if' cond l r        = inj @@ Fmap5.distrib @@ Term.If (cond, l, r)
let repeat t            = inj @@ Fmap5.distrib @@ Term.Repeat t
let read mo l           = inj @@ Fmap5.distrib @@ Term.Read (mo, l)
let write mo l t        = inj @@ Fmap5.distrib @@ Term.Write (mo, l, t)
let cas mo1 mo2 l t1 t2 = inj @@ Fmap5.distrib @@ Term.Cas (mo1, mo2, l, t1, t2)
let seq t1 t2           = inj @@ Fmap5.distrib @@ Term.Seq (t1, t2)
let spw t1 t2           = inj @@ Fmap5.distrib @@ Term.Spw (t1, t2)
let par t1 t2           = inj @@ Fmap5.distrib @@ Term.Par (t1, t2)
let skip                = inj @@ Fmap5.distrib @@ Term.Skip
let stuck               = inj @@ Fmap5.distrib @@ Term.Stuck

module Context =
  struct
    type ('expr, 'string, 'mo, 'loc, 't, 'c) t =
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

    type tt   = (int, string, mem_order, loc, Term.tt, tt) t
    type tl  = (MiniKanren.Nat.logic, string MiniKanren.logic, mem_order MiniKanren.logic, loc MiniKanren.logic, Term.tl, tl) t MiniKanren.logic
    type ti  = (tt, tl) MiniKanren.injected

    let fmap fint fstring fmo floc ft fc c = failwith "Not implemented"
  end

module Fmap6 = Fmap6(Context)

let binop_left op l r       = inj @@ Fmap6.distrib @@ Context.BinopL (op, l, r)
let binop_right op l r      = inj @@ Fmap6.distrib @@ Context.BinopR (op, l, r)
let pair_left l r           = inj @@ Fmap6.distrib @@ Context.PairL (l, r)
let pair_right l r          = inj @@ Fmap6.distrib @@ Context.PairR (l, r)
let asgn_ctx l r            = inj @@ Fmap6.distrib @@ Context.AsgnC (l, r)
let write_ctx mo l t        = inj @@ Fmap6.distrib @@ Context.WriteC (mo, l, t)
let if_ctx cond l r         = inj @@ Fmap6.distrib @@ Context.IfC (cond, l, r)
let seq_ctx t1 t2           = inj @@ Fmap6.distrib @@ Context.SeqC (t1, t2)
let par_left t1 t2          = inj @@ Fmap6.distrib @@ Context.ParL (t1, t2)
let par_right t1 t2         = inj @@ Fmap6.distrib @@ Context.ParR (t1, t2)
let hole                    = inj @@ Fmap6.distrib @@ Context.Hole

type t   = Term.tt
type tl  = Term.tl
type ti  = Term.ti

type c   = Context.tt
type cl  = Context.tl
type ci  = Context.ti

let rec preallocate' vars atomics = Term.(function
  | Read  (_, x)
  | Write (_, x, _) ->
    if List.mem x atomics then (vars, atomics) else (vars, x::atomics)
  | Var x ->
    if List.mem x vars then (vars, atomics) else (x::vars, atomics)
  | Repeat t ->
    preallocate' vars atomics t
  | Binop (_, t1, t2)
  | Asgn  (t1, t2)
  | Pair  (t1, t2)
  | Seq   (t1, t2)
  | Spw   (t1, t2)
  | Par   (t1, t2) ->
    let (vars', atomics') = preallocate' vars atomics t1 in
      preallocate' vars' atomics' t2
  | If (t1, t2, t3) ->
    let (vars' , atomics')  = preallocate' vars atomics t1 in
    let (vars'', atomics'') = preallocate' vars' atomics' t2 in
    preallocate' vars'' atomics'' t3
  | _  -> (vars, atomics)
)

let preallocate = preallocate' [] []

(* let inj_term t = let module Fmap = Fmap5(Term) in lift @@ Fmap.distrib t *)
let rec inj_term t = Term.fmap (!!) (!!) (!!) (!!) (inj_term) t

let rec inj_context c = Context.fmap (!!) (!!) (!!) (!!) inj_term inj_context c

let (!) = (!!)

let rec reducibleo t b = Term.(conde [
  fresh (n)
    (b === !false)
    (t === const n);
  fresh (x)
    (b === !true)
    (t === var x);
  fresh (op l r)
    (b === !true)
    (t === binop op l r);
  fresh (l r)
    (b === !true)
    (t === asgn l r);
  fresh (e t1 t2)
    (b === !true)
    (t === if' e t1 t2);
  fresh (t')
    (b === !true)
    (t === repeat t');
  fresh (mo l)
    (b === !true)
    (t === read mo l);
  fresh (mo l t')
    (b === !true)
    (t === write mo l t');
  fresh (mo1 mo2 l e1 e2)
    (b === !true)
    (t === cas mo1 mo2 l e1 e2);
  fresh (t1 t2)
    (b === !true)
    (t === seq t1 t2);
  fresh (t1 t2)
    (b === !true)
    (t === spw t1 t2);
  fresh (t1 t2)
    (b === !true)
    (t === par t1 t2);

  (conde [
     fresh (t1 t2 b1 b2)
       (t === pair t1 t2)
       (reducibleo t1 b1)
       (reducibleo t2 b2)
       (Bool.oro b1 b2 b)
  ]);

  ((b === !false) &&& (t === skip));
  ((b === !false) &&& (t === stuck));
])

let rec splito t c rdx = Term.(Context.(conde [
  fresh (op l r c' t')
    (t === binop op l r)
    (conde [
      ((c === hole) &&& (reducibleo l !false) &&& (reducibleo r !false) &&& (rdx === t));
      ((c === binop_left op c' r) &&& (reducibleo l !true)
        &&& (rdx === t') &&& (splito l c' t'));
      ((c === binop_right op l c') &&& (reducibleo l !false) &&& (reducibleo r !true)
        &&& (rdx === t') &&& (splito r c' t'));
    ]);

  fresh (t1 t2 c' t')
    (t === pair t1 t2)
    (conde [
      ((c === hole)             &&& (reducibleo t1 !false) &&& (reducibleo t2 !false) &&& (rdx === t));
      ((c === pair_left c' t2)  &&& (reducibleo t1 !true)
        &&& (rdx === t') &&& (splito t1 c' t'));
      ((c === pair_right t1 c') &&& (reducibleo t1 !false) &&& (reducibleo t2 !true)
        &&& (rdx === t') &&& (splito t2 c' t'));
    ]);

  fresh (l r c' t')
    (t === asgn l r)
    (conde [
      ((c === hole)            &&& (rdx === t ));
      ((c === asgn_ctx l c') &&& (rdx === t') &&& (splito r c' t'));
    ]);

  fresh (mo loc e c' t')
    (t === write mo loc e)
    (conde [
      ((c === hole)                &&& (rdx === t ));
      ((c === write_ctx mo loc c') &&& (rdx === t') &&& (splito e c' t'));
    ]);

  fresh (cond btrue bfalse c' t')
    (t === if' cond btrue bfalse)
    (conde [
      ((c === hole)                   &&& (rdx === t ));
      ((c === if_ctx c' btrue bfalse) &&& (rdx === t') &&& (splito cond c' t'))
    ]);

  fresh (t1 t2 c' t')
    (t === seq t1 t2)
    (c === seq_ctx c' t2)
    (rdx === t')
    (splito t1 c' t');

  fresh (t1 t2 c' t')
    (t === par t1 t2)
    (conde [
       ((c === hole)            &&& (rdx === t ));
       ((c === par_left  c' t2) &&& (rdx === t') &&& (splito t1 c' t'));
       ((c === par_right t1 c') &&& (rdx === t') &&& (splito t2 c' t'));
    ]);

  ((c === hole) &&& (rdx === t) &&& conde [
    fresh (n)
      (t === const n);

    fresh (x)
      (t === var x);

    fresh (t')
      (t === repeat t');

    fresh (mo l)
      (t === read mo l);

    fresh (mo1 mo2 l t1 t2)
      (t === cas mo1 mo2 l t1 t2);

    fresh (t1 t2)
      (t === spw t1 t2);

    (t === skip);

    (t === stuck);
  ]);
]))

  let rec plugo t c rdx = Term.(Context.(
    (conde [
      fresh (op l r c' t')
        (t === binop op l r)
        (conde [
          ((c === hole)                 &&& (rdx === t));
          ((c === binop_left  op c' r)  &&& (rdx === t') &&& (plugo l c' t'));
          ((c === binop_right op l c')  &&& (rdx === t') &&& (plugo r c' t'));
        ]);

      fresh (t1 t2 c' t')
        (t === pair t1 t2)
        (conde [
          ((c === hole)             &&& (rdx === t ));
          ((c === pair_left  c' t2) &&& (rdx === t') &&& (plugo t1 c' t'));
          ((c === pair_right t1 c') &&& (rdx === t') &&& (plugo t2 c' t'));
        ]);

      fresh (l r c' t')
        (t === asgn l r)
        (conde [
          ((c === hole)          &&& (rdx === t ));
          ((c === asgn_ctx l c') &&& (rdx === t') &&& (plugo r c' t'));
        ]);

      fresh (mo loc e c' t')
        (t === write mo loc e)
        (conde [
          ((c === hole)                &&& (rdx === t ));
          ((c === write_ctx mo loc c') &&& (rdx === t') &&& (plugo e c' t'));
        ]);

      fresh (cond btrue bfalse c' t')
        (t === if' cond btrue bfalse)
        (conde [
          ((c === hole)                   &&& (rdx === t ));
          ((c === if_ctx c' btrue bfalse) &&& (rdx === t') &&& (plugo cond c' t'))
        ]);

      fresh (t1 t2 c')
        ((c === seq_ctx c' t2) &&& conde [
          (rdx =/= skip) &&& (rdx =/= stuck) &&& (t === seq t1 t2) &&& (plugo t1 c' rdx);
          (rdx === skip)  &&& (t === t2);
          (rdx === stuck) &&& (t === stuck);
        ]);

      fresh (t1 t2 c' t')
        (t === par t1 t2)
        (conde [
           ((c === hole)            &&& (rdx === t ));
           ((c === par_left  c' t2) &&& (rdx === t') &&& (plugo t1 c' t'));
           ((c === par_right t1 c') &&& (rdx === t') &&& (plugo t2 c' t'));
        ]);

      ((c === hole) &&& (rdx === t) &&& conde [
        fresh (n)
          (t === const n);

        fresh (x)
          (t === var x);

        fresh (t')
          (t === repeat t');

        fresh (mo l)
          (t === read mo l);

        fresh (mo1 mo2 l t1 t2)
          (t === cas mo1 mo2 l t1 t2);

        fresh (t1 t2)
          (t === spw t1 t2);

        (t === skip);

        (t === stuck);
      ]);
    ])))

let rec patho c path = Term.(Context.(
    fresh (op mo loc t1 t2 t3 t' c' path')
      (conde [
        (c === hole)                  &&& (path === pathn);
        (c === binop_left op c' t1)   &&& (patho c' path);
        (c === binop_right op t1 c')  &&& (patho c' path);
        (c === pair_left c' t2)       &&& (patho c' path);
        (c === pair_right t1 c')      &&& (patho c' path);
        (c === asgn_ctx t1 c')        &&& (patho c' path);
        (c === write_ctx mo loc c')   &&& (patho c' path);
        (c === if_ctx c' t2 t3)       &&& (patho c' path);
        (c === seq_ctx c' t1)         &&& (patho c' path);
        (c === par_left c' t1)        &&& (path === pathl path') &&& (patho c' path');
        (c === par_right t1 c')       &&& (path === pathr path') &&& (patho c' path');
      ])
  ))
