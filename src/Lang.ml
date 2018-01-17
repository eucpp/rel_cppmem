open MiniKanren
(* open MiniKanrenStd *)
open Utils

module Reg =
  struct
    type tt = string

    type tl = inner MiniKanren.logic
      and inner = string

    type ti = (tt, tl) MiniKanren.injected

    let reg r = !!r

    let reify = reify

    let show = GT.show(logic) (fun s -> s)

    let pprint ff r = Format.fprintf ff "%s" @@ show r
  end

module Loc =
  struct
    type tt = string

    type tl = inner MiniKanren.logic
      and inner = string

    type ti = (tt, tl) MiniKanren.injected

    let loc l = !!l

    let reify = reify

    let show = GT.show(logic) (fun s -> s)

    let pprint ff l = Format.fprintf ff "%s" @@ show l
  end

module Value =
  struct
    type tt = tt Std.nat

    type tl = inner MiniKanren.logic
      and inner = tl Std.nat

    type ti = MiniKanrenStd.Nat.groundi

    let integer = Std.nat

    let reify = Std.Nat.reify

    let rec show n =
      let rec to_ground : tl -> int = function
      | Value (S n) -> 1 + (to_ground n)
      | Value (O)   -> 0
      | Var (i, _)  -> invalid_arg "Free Var"
      in
      try
        string_of_int @@ to_ground n
      with Invalid_argument _ ->
        match n with
        | Value (S n) ->
          Printf.sprintf "_.??{=/= 0}"
        | Var (i, []) ->
          Printf.sprintf "_.%d" i
        | Var (i, cs) ->
          let cs = String.concat "; " @@ List.map show cs in
          Printf.sprintf "_.%d{=/= %s}" i cs

    let pprint ff v = Format.fprintf ff "%s" @@ show v
    let nullo v = (v === Std.Nat.zero)

    let not_nullo v = fresh (x) (v === Std.Nat.succ x)


    let addo = Std.Nat.addo
    let mulo = Std.Nat.mulo

    let eqo = (===)
    let nqo = (=/=)
    let lto = Std.Nat.(<)
    let leo = Std.Nat.(<=)
    let gto = Std.Nat.(>)
    let geo = Std.Nat.(>=)
  end

module MemOrder =
  struct
    type tt = SC | ACQ | REL | ACQ_REL | CON | RLX | NA

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    let of_string = function
      | "sc"      -> SC
      | "acq"     -> ACQ
      | "rel"     -> REL
      | "relAcq"  -> ACQ_REL
      | "con"     -> CON
      | "rlx"     -> RLX
      | "na"      -> NA

    let to_string = function
      | SC      -> "sc"
      | ACQ     -> "acq"
      | REL     -> "rel"
      | ACQ_REL -> "relAcq"
      | CON     -> "con"
      | RLX     -> "rlx"
      | NA      -> "na"

    let mo s = !!(of_string s)

    let reify = reify

    let inj = to_logic

    let show = GT.show(logic) (to_string)

    let pprint ff mo = Format.fprintf ff "%s" @@ show mo
  end

module Uop =
  struct
    type tt = NOT

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    let of_string = function
      | "!"   -> NOT

    let to_string = function
      | NOT   -> "!"

    let uop s = !!(of_string s)

    let reify = reify

    let inj = to_logic

    let show = GT.show(logic) (to_string)

    let pprint ff op = Format.fprintf ff "%s" @@ show op
  end

module Bop =
  struct
    type tt = ADD | MUL | EQ | NEQ | LT | LE | GT | GE | OR | AND

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected

    let of_string = function
      | "+"   -> ADD
      | "*"   -> MUL
      | "="   -> EQ
      | "!="  -> NEQ
      | "<"   -> LT
      | "<="  -> LE
      | ">"   -> GT
      | ">="  -> GE
      | "||"  -> OR
      | "&&"  -> AND

    let to_string = function
      | ADD   -> "+"
      | MUL   -> "*"
      | EQ    -> "="
      | NEQ   -> "!="
      | LT    -> "<"
      | LE    -> "<="
      | GT    -> ">"
      | GE    -> ">="
      | OR    -> "||"
      | AND   -> "&&"

    let bop s = !!(of_string s)

    let reify = reify

    let inj = to_logic

    let show = GT.show(logic) (to_string)

    let pprint ff op = Format.fprintf ff "%s" @@ show op
  end

module ThreadID =
  struct
    type tt = tt Std.nat

    type tl = inner MiniKanren.logic
      and inner = tl Std.nat

    type ti = MiniKanrenStd.Nat.groundi

    let tid = Std.nat

    let null = Std.Nat.zero
    let init = Std.Nat.one
    let next = Std.Nat.succ

    let reify = Std.Nat.reify

    let rec show n =
      let rec to_ground : tl -> int = function
      | Value (S n) -> 1 + (to_ground n)
      | Value (O)   -> 0
      | Var (i, _)  -> invalid_arg "Free Var"
      in
      try
        string_of_int @@ to_ground n
      with Invalid_argument _ -> Printf.sprintf "_.??"

    let pprint ff v = Format.fprintf ff "%s" @@ show v
  end

module RegStorage =
  struct
    type tt = (Reg.tt, Value.tt) Storage.tt

    type tl = (Reg.tl, Value.tl) Storage.tl
      and inner = (Reg.tl, Value.tl) Storage.inner

    type ti = (Reg.tt, Value.tt, Reg.tl, Value.tl) Storage.ti

    let empty = Storage.empty

    let init rs = Storage.allocate (Value.integer 0) @@ List.map Reg.reg rs

    let from_assoc xs =
      Storage.from_assoc @@ List.map (fun (r, v) -> (Reg.reg r, Value.integer v)) xs

    let reify = Storage.reify (Reg.reify) (Value.reify)

    let pprint =
      Storage.pprint (fun ff (k, v) -> Format.fprintf ff "%s=%s" (Reg.show k) (Value.show v))

    let reado  = Storage.geto
    let writeo = Storage.seto

    let checko t rvs = Storage.checko t @@ List.map (fun (r, v) -> (Reg.reg r, Value.integer v)) rvs
  end

module Expr =
  struct
    module T =
      struct
        @type ('var, 'value, 'uop, 'bop, 't) t =
          | Var     of 'var
          | Const   of 'value
          | Unop    of 'uop * 't
          | Binop   of 'bop * 't * 't
        with gmap, show

        let fmap fa fb fc fd fe x = GT.gmap(t) fa fb fc fd fe x
      end

    type tt = (Reg.tt, Value.tt, Uop.tt, Bop.tt, tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (Reg.tl, Value.tl, Uop.tl, Bop.tl, tl) T.t

    type ti = (tt, tl) Semantics.Term.ti

    module FT = Fmap5(T)

    let var x         = inj @@ FT.distrib @@ T.Var x
    let const v       = inj @@ FT.distrib @@ T.Const v
    let unop op e     = inj @@ FT.distrib @@ T.Unop (op, e)
    let binop op l r  = inj @@ FT.distrib @@ T.Binop (op, l, r)

    let rec reify h = FT.reify (Reg.reify) (Value.reify) (Uop.reify) (Bop.reify) reify h

    let rec show t =
      GT.show(logic) (GT.show(T.t) Reg.show Value.show Uop.show Bop.show show) t

    let rec pprint ff x = T.(
      let s ff = function
        | Var x            -> Format.fprintf ff "@[%a@]" Reg.pprint x
        | Const n          -> Format.fprintf ff "@[%a@]" Value.pprint n
        | Unop (op, e)     -> Format.fprintf ff "@[%a (%a)@]" Uop.pprint op pprint e
        | Binop (op, l, r) -> Format.fprintf ff "@[%a %a %a@]" pprint l Bop.pprint op pprint r
      in
      pprint_logic s ff x
    )

    let rec evalo rs e v = Value.(conde [
      (e === const v);

      fresh (x)
        (e === var x)
        (RegStorage.reado rs x v);

      fresh (op e' v')
        (e === unop !!Uop.NOT e')
        (conde [
          (Value.nullo v')     &&& (v === (integer 1));
          (Value.not_nullo v') &&& (v === (integer 0));
        ])
        (evalo rs e' v');

      fresh (op l r x y)
        (e === binop op l r)
        (evalo rs l x)
        (evalo rs r y)
        (conde [
          (op === !!Bop.ADD) &&& (addo x y v);
          (op === !!Bop.MUL) &&& (mulo x y v);
          (op === !!Bop.EQ ) &&& (conde [(eqo x y) &&& (v === (integer 1)); (nqo x y) &&& (v === (integer 0))]);
          (op === !!Bop.NEQ) &&& (conde [(nqo x y) &&& (v === (integer 1)); (eqo x y) &&& (v === (integer 0))]);
          (op === !!Bop.LT ) &&& (conde [(lto x y) &&& (v === (integer 1)); (geo x y) &&& (v === (integer 0))]);
          (op === !!Bop.LE ) &&& (conde [(leo x y) &&& (v === (integer 1)); (gto x y) &&& (v === (integer 0))]);
          (op === !!Bop.GT ) &&& (conde [(gto x y) &&& (v === (integer 1)); (leo x y) &&& (v === (integer 0))]);
          (op === !!Bop.GE ) &&& (conde [(geo x y) &&& (v === (integer 1)); (lto x y) &&& (v === (integer 0))]);

          (op === !!Bop.OR ) &&& (conde [
            (nullo x)     &&& (nullo y)     &&& (v === (integer 0));
            (not_nullo x) &&& (nullo y)     &&& (v === (integer 1));
            (nullo x)     &&& (not_nullo y) &&& (v === (integer 1));
            (not_nullo x) &&& (not_nullo y) &&& (v === (integer 1));
          ]);

          (op === !!Bop.AND) &&& (conde [
            (nullo x)     &&& (nullo y)     &&& (v === (integer 0));
            (not_nullo x) &&& (nullo y)     &&& (v === (integer 0));
            (nullo x)     &&& (not_nullo y) &&& (v === (integer 0));
            (not_nullo x) &&& (not_nullo y) &&& (v === (integer 1));
          ]);
        ])
    ])

  end

module Stmt =
  struct
    module T =
      struct
        @type ('reg, 'regs, 'loc, 'mo, 'e, 'ts) t =
          | Assert   of 'e
          | Asgn     of 'reg * 'e
          | If       of 'e * 'ts * 'ts
          | While    of 'e * 'ts
          | Repeat   of 'ts * 'e
          | Load     of 'mo * 'loc * 'reg
          | Store    of 'mo * 'loc * 'e
          | Cas      of 'mo * 'mo * 'loc * 'e * 'e
          | Spw      of 'ts * 'ts
          | Return   of 'regs
        with gmap, show

        let fmap fa fb fc fd fe ff x = GT.gmap(t) fa fb fc fd fe ff x
      end

    type tt = (Reg.tt, Reg.tt Std.List.ground, Loc.tt, MemOrder.tt, Expr.tt, tt Std.List.ground) T.t

    type tl = inner MiniKanren.logic
      and inner = (Reg.tl, Reg.tl Std.List.logic, Loc.tl, MemOrder.tl, Expr.tl, tl Std.List.logic) T.t

    type ti = (tt, tl) Semantics.Term.ti

    module FT = Fmap6(T)

    let assertion e         = inj @@ FT.distrib @@ T.Assert e
    let asgn r e            = inj @@ FT.distrib @@ T.Asgn (r, e)
    let if' e l r           = inj @@ FT.distrib @@ T.If (e, l, r)
    let while' e t          = inj @@ FT.distrib @@ T.While (e, t)
    let repeat t e          = inj @@ FT.distrib @@ T.Repeat (t, e)
    let load mo l r         = inj @@ FT.distrib @@ T.Load (mo, l, r)
    let store mo l t        = inj @@ FT.distrib @@ T.Store (mo, l, t)
    let cas mo1 mo2 l e1 e2 = inj @@ FT.distrib @@ T.Cas (mo1, mo2, l, e1, e2)
    let spw t1 t2           = inj @@ FT.distrib @@ T.Spw (t1, t2)
    let return rs           = inj @@ FT.distrib @@ T.Return rs

    let skip = Std.nil
    let single s = Std.(%) s (Std.nil ())

    let seqo = Std.List.appendo

    let rec reify h =
      FT.reify Reg.reify (Std.List.reify Reg.reify) Loc.reify MemOrder.reify Expr.reify (Std.List.reify reify) h

    let rec show t =
      GT.show(logic) (GT.show(T.t) Reg.show (GT.show(Std.List.logic) Reg.show) Loc.show MemOrder.show Expr.show (GT.show(Std.List.logic) show)) t

    let rec pprint ff x = Format.fprintf ff "%a@." ppl x
    and ppl ff x = pprint_logic pp ff x
    and ppseql ff x = pprint_logic ppseq ff x
    and ppseq ff =
      let open Std in function
      | Cons (t, ts) -> Format.fprintf ff "@[<v>%a;@;%a@]" ppl t ppseql ts
      | Nil          -> ()
    and pp ff = T.(function
      | Assert e                ->
        Format.fprintf ff "@[assert (%a)@;@]" Expr.pprint e
      | Asgn (r, e)             ->
        Format.fprintf ff "@[%a := %a@]" Reg.pprint r Expr.pprint e
      | If (e, t1, t2)          ->
        Format.fprintf ff "@[<v>if %a@;then %a@;else %a@]" Expr.pprint e ppseql t1 ppseql t2
      | While (e, t)            ->
        Format.fprintf ff "@[while (%a) @;<1 4>%a@;@]" Expr.pprint e ppseql t
      | Repeat (t, e)           ->
        Format.fprintf ff "@[repeat @;<1 4>%a@; until (%a)@]" ppseql t Expr.pprint e
      | Load (m, l, r)          ->
        Format.fprintf ff "@[%a := %a_%a@]" Reg.pprint r Loc.pprint l MemOrder.pprint m
      | Store (m, l, e)         ->
        Format.fprintf ff "@[%a_%a :=@;<1 4>%a@]" Loc.pprint l MemOrder.pprint m Expr.pprint e
      | Cas (m1, m2, l, e, d)   ->
        Format.fprintf ff "@[cas_%a_%a(%a, %a, %a)@]" MemOrder.pprint m1 MemOrder.pprint m2 Loc.pprint l Expr.pprint e Expr.pprint d
      | Spw (t1, t2)            ->
        Format.fprintf ff "@[<v>spw {{{@;<1 4>%a@;|||@;<1 4>%a@;}}}@]" ppseql t1 ppseql t2
      | Return rs               ->
        Format.fprintf ff "@[<v>return %s@]" (GT.show(Std.List.logic) Reg.show rs)
    )

  end

module Prog =
  struct
    type tt = Stmt.tt Std.List.ground

    type tl = inner MiniKanren.logic
      and inner = (Stmt.tl, tl) Std.list

    type ti = (Stmt.tt, Stmt.tl) Std.List.groundi

    let reify = Std.List.reify Stmt.reify

    let pprint = Stmt.ppseql
  end

let prog p = Std.List.list p

module Error =
  struct
    module T =
      struct
        @type ('expr, 'mo, 'loc) t =
          | Assertion       of 'expr
          | Datarace        of 'mo * 'loc
        with gmap, show

        let fmap fa fb fc x = GT.gmap(t) fa fb fc x
      end

    type tt = (Expr.tt, MemOrder.tt, Loc.tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (Expr.tl, MemOrder.tl, Loc.tl) T.t

    type ti = (tt, tl) Semantics.Term.ti

    module F = Fmap3(T)

    let rec reify h =
      F.reify Expr.reify MemOrder.reify Loc.reify h

    let rec show t =
      GT.show(logic) (GT.show(T.t) Expr.show MemOrder.show Loc.show) t

    let pprint ff x = Format.fprintf ff "@[%s@]" (show x)

    let assertion e       = inj @@ F.distrib @@ T.Assertion e
    let datarace mo loc   = inj @@ F.distrib @@ T.Datarace (mo, loc)
  end

module Label =
  struct
    module T =
      struct
        @type ('tid, 'mo, 'loc, 'value, 'err) t =
          | Empty
          | Load            of 'mo * 'loc * 'value
          | Store           of 'mo * 'loc * 'value
          | CAS             of 'mo * 'mo * 'loc * 'value * 'value * 'value
          | Error           of 'err
        with gmap, show

        let fmap fa fb fc fd fe x = GT.gmap(t) fa fb fc fd fe x
      end

    type tt = (ThreadID.tt, MemOrder.tt, Loc.tt, Value.tt, Error.tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (ThreadID.tl, MemOrder.tl, Loc.tl, Value.tl, Error.tl) T.t

    type ti = (tt, tl) MiniKanren.injected

    module FT = Fmap5(T)

    let empty ()               = inj @@ FT.distrib @@ T.Empty
    let load  mo loc v         = inj @@ FT.distrib @@ T.Load  (mo, loc, v)
    let store mo loc v         = inj @@ FT.distrib @@ T.Store (mo, loc, v)
    let cas mo1 mo2 loc e d v  = inj @@ FT.distrib @@ T.CAS (mo1, mo2, loc, e, d, v)
    let error e                = inj @@ FT.distrib @@ T.Error e

    let reify h = FT.reify ThreadID.reify MemOrder.reify Loc.reify Value.reify Error.reify h

    let show = GT.show(T.t) ThreadID.show MemOrder.show Loc.show Value.show Error.show

    let pprint = pprint_logic @@ (fun ff x -> Format.fprintf ff "@[%s@]" (show x))

    let erroro label opterr = conde
      [ fresh (err)
          (label  === error err)
          (opterr === Std.some err)

      ; ?~(fresh (err) (label === error err)) &&& (opterr === Std.none ())
      ]

  end

module Thread =
  struct
    module T =
      struct
        @type ('regs, 'prog, 'tid) t =
          { regs : 'regs
          ; prog : 'prog
          ; pid  : 'tid
          }
        with gmap

        let fmap fa fb fc x = GT.gmap(t) fa fb fc x
      end

    type tt = (RegStorage.tt, Stmt.tt Std.List.ground, ThreadID.tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (RegStorage.tl, Stmt.tl Std.List.logic, ThreadID.tl) T.t

    type ti = (tt, tl) MiniKanren.injected

    module F = Fmap3(T)

    let thrd prog regs pid = T.(inj @@ F.distrib {prog; regs; pid})

    let init ~prog ~regs ~pid = thrd prog regs pid

    let reify h = F.reify RegStorage.reify Prog.reify ThreadID.reify h

    let pprint =
      let pp ff = let open T in fun { regs; prog; pid; } ->
        Format.fprintf ff "@[<v>pid: %a@;@[<v>Code :@;<1 4>%a@;@]@;@[<v>Regs :@;<1 4>%a@;@]@;@]"
          ThreadID.pprint pid
          Stmt.ppseql prog
          RegStorage.pprint regs
      in
      pprint_logic pp

    open Std
    open Stmt

    let regso t rs =
      fresh (prog pid)
        (t  === thrd prog rs pid)

    let terminatedo t =
      fresh (rs pid)
        (t  === thrd (Stmt.skip ()) rs pid)

    let asserto label t t' =
      fresh (prog prog' rs pid e v stmts)
        (t  === thrd prog  rs pid)
        (t' === thrd prog' rs pid)
        (prog === (assertion e) % stmts)
        (Expr.evalo rs e v)
        (conde [
          (Value.not_nullo v) &&& (prog' === stmts  ) &&& (label === Label.empty ());
          (Value.nullo     v) &&& (prog' === skip ()) &&& (label === Label.error (Error.assertion e));
        ])

    let asgno label t t' =
      fresh (prog prog' rs rs' pid r e v)
        (t  === thrd prog  rs  pid)
        (t' === thrd prog' rs' pid)
        (prog === (asgn r e) % prog')
        (label === Label.empty ())
        (Expr.evalo rs e v)
        (RegStorage.writeo rs rs' r v)

    let ifo label t t' =
      fresh (prog prog' rs pid e v t1 t2 stmts)
        (t  === thrd prog  rs pid)
        (t' === thrd prog' rs pid)
        (prog === (if' e t1 t2) % stmts)
        (label === Label.empty ())
        (Expr.evalo rs e v)
        (conde [
          (Value.not_nullo v) &&& (seqo t1 stmts prog');
          (Value.nullo     v) &&& (seqo t2 stmts prog');
        ])

    let whileo label t t' =
      fresh (prog prog' rs pid e body stmt stmt' stmts stmts')
        (t  === thrd prog  rs pid)
        (t' === thrd prog' rs pid)
        (prog  === stmt  % stmts)
        (prog' === stmt' % stmts)
        (stmt  === (while' e body))
        (stmt' === if' e stmts' (skip ()))
        (label === Label.empty ())
        (seqo body (single stmt) stmts')

    let repeato label t t' =
      fresh (prog prog' rs pid e body body' stmts)
        (t  === thrd prog  rs pid)
        (t' === thrd prog' rs pid)
        (prog  === (repeat body e) % stmts)
        (label === Label.empty ())
        (seqo body (single @@ while' (Expr.unop !!Uop.NOT e) body) body')
        (seqo body' stmts prog')

    let loado label t t' =
      fresh (prog prog' rs rs' pid mo l r v)
        (t  === thrd prog  rs  pid)
        (t' === thrd prog' rs' pid)
        (prog === (load mo l r) % prog')
        (label === Label.load mo l v)
        (RegStorage.writeo rs rs' r v)

    let storeo label t t' =
      fresh (prog prog' rs pid mo l e v)
        (t  === thrd prog  rs pid)
        (t' === thrd prog' rs pid)
        (prog === (store mo l e) % prog')
        (label === Label.store mo l v)
        (Expr.evalo rs e v)

    let dataraceo label t t' =
      fresh (prog prog' rs pid stmt mo l r e)
        (t  === thrd prog  rs pid)
        (t' === thrd prog' rs pid)
        (prog === stmt % prog')
        (label === Label.error (Error.datarace mo l))
        ((stmt === load mo l r) ||| (stmt === store mo l e))

    let stepo label t t' =
      let rules =
        [asserto; asgno; ifo; whileo; repeato; loado; storeo; dataraceo]
      in
      conde @@ List.map (fun rule -> rule label t t') rules

  end

module ThreadLocalStorage(T : Utils.Logic) =
  struct
    type tt = ThreadID.tt * (ThreadID.tt, T.tt) Storage.tt

    type tl = inner MiniKanren.logic
      and inner = ThreadID.tl * (ThreadID.tl, T.tl) Storage.tl

    type ti = (tt, tl) MiniKanren.injected

    let reify = Std.Pair.reify ThreadID.reify (Storage.reify ThreadID.reify T.reify)

    let pprint =
      let pp ff (_, thrds) = Storage.pprint (
        fun ff (tid, thrd) ->
          Format.fprintf ff "@[<v>Thread #%a:@;<1 2>%a@]" ThreadID.pprint tid T.pprint thrd
        ) ff thrds
      in
      pprint_logic pp

    let of_list thrds =
      let cnt, ts = ListLabels.fold_left thrds ~init:(ThreadID.init, [])
        ~f:(fun (tid, acc) thrd -> (ThreadID.next tid, (tid, thrd)::acc))
      in
      let thrds = Storage.from_assoc ts in
      Std.pair cnt thrds

    let initi n make_thrd =
      let rec helper i xs =
        let thrd = make_thrd @@ ThreadID.tid i in
        if i = n then xs else helper (i+1) (thrd::xs)
      in
      of_list @@ helper 0 []

    let init n thrd =
      initi n (fun _ -> thrd)

    let geto tls tid thrd =
      fresh (cnt thrds)
        (tls === Std.pair cnt thrds)
        (Storage.membero thrds tid thrd)

    let seto tls tls' tid thrd =
      fresh (cnt thrds thrds')
        (tls  === Std.pair cnt thrds )
        (tls' === Std.pair cnt thrds')
        (Storage.seto thrds thrds' tid thrd)

    let foldo g tls acc acc' =
      fresh (cnt thrds)
        (tls === Std.pair cnt thrds)
        (Storage.foldo (fun _ -> g) thrds acc acc')

    let forallo g tls =
      fresh (cnt thrds)
        (tls === Std.pair cnt thrds)
        (Storage.forallo (fun _ thrd -> g thrd) thrds)

  end

module ThreadManager = ThreadLocalStorage(Thread)

module type MemoryModel =
  sig
    include Utils.Logic

    val init : thrdn:int -> mem:(string * int) list -> ti

    val stepo : ThreadID.ti -> Label.ti -> ti -> ti -> MiniKanren.goal
  end

module State(Memory : MemoryModel) =
  struct
    module T =
      struct
        @type ('thrdm, 'mem, 'opterr) t =
          { thrdm  : 'thrdm
          ; mem    : 'mem
          ; opterr : 'opterr
          }
        with gmap

        let fmap fa fb fc x = GT.gmap(t) fa fb fc x
      end

    type tt = (ThreadManager.tt, Memory.tt, Error.tt Std.Option.ground) T.t

    type tl = inner MiniKanren.logic
      and inner = (ThreadManager.tl, Memory.tl, Error.tl Std.Option.logic) T.t

    type ti = (tt, tl) MiniKanren.injected

    module F = Fmap3(T)

    let reify = F.reify
      ThreadManager.reify
      Memory.reify
      (Std.Option.reify Error.reify)

    let pprint =
      let pp ff = let open T in fun {thrdm; mem; opterr} ->
        pprint_logic (fun ff -> function
          | None      -> ()
          | Some err  ->
            Format.fprintf ff "@[<v>Error:%a@]@;" Error.pprint err
        ) ff opterr;
        Format.fprintf ff "@[<v>Threads:@;<1 2>%a@]@;@[<v>Memory:@;<1 2>%a@]@."
          ThreadManager.pprint thrdm
          Memory.pprint mem
      in
      pprint_logic pp

    let state thrdm mem opterr = T.(inj @@ F.distrib {thrdm; mem; opterr})

    let init thrdm mem = state thrdm mem (Std.none ())

    let thrdo ?err s tid thrd =
      fresh (thrdm mem)
        (s === state thrdm mem (Std.Option.option err))
        (ThreadManager.geto thrdm tid thrd)

    let memo ?err s mem =
      fresh (thrdm opterr)
        (s === state thrdm mem (Std.Option.option err))

    let erroro s err =
      fresh (thrdm mem)
        (s === state thrdm mem (Std.some err))

    let terminatedo s =
      fresh (thrdm mem opterr)
        (s === state thrdm mem opterr)
        (conde [
          fresh (err)
            (opterr === Std.some err);

            (opterr === Std.none ()) &&& (ThreadManager.forallo Thread.terminatedo thrdm);
        ])

    let stepo s s' =
      fresh (thrdm thrdm' mem mem' opterr thrd thrd' tid label)
        (s  === state thrdm  mem  (Std.none ()))
        (s' === state thrdm' mem' opterr)
        (ThreadManager.geto thrdm tid thrd)
        (conde [
          ?&[ (Thread.stepo label thrd thrd')
            ; (Memory.stepo tid label mem mem')
            ; (ThreadManager.seto thrdm thrdm' tid thrd')
            ; (Label.erroro label opterr)
            ]
        ])

    let evalo = Semantics.Reduction.make_eval ~irreducibleo:terminatedo stepo

  end

module SeqInterpreter =
  struct
    module DummyMM =
      struct
        type tt = unit
        type tl = inner MiniKanren.logic
          and inner = unit
        type ti = (tt, tl) MiniKanren.injected

        let reify = MiniKanren.reify

        let pprint ff t = ()

        let init ~thrdn ~mem = !!()

        let stepo tid label t t' =
          (t === t') &&&
          (conde
            [ (label === Label.empty ())
            ; fresh (err) (label === Label.error @@ Error.assertion err)
            ]
          )
      end

    module State =
      struct
        include State(DummyMM)

        let state prog regs opterr =
          let thrd  = Thread.init ~prog ~regs ~pid:ThreadID.null in
          let thrdm = ThreadManager.init 1 thrd in
          state thrdm !!() opterr

        let init prog regs = state prog regs (Std.none ())

        let regso s rs =
          fresh (thrd)
            (thrdo s ThreadID.init thrd)
            (Thread.regso thrd rs)
      end

    module Result =
      struct
        type tt = RegStorage.tt * Error.tt Std.Option.ground

        type tl = inner MiniKanren.logic
          and inner = RegStorage.tl * Error.tl Std.Option.logic

        type ti = (tt, tl) MiniKanren.injected

        let reify = Std.Pair.reify RegStorage.reify (Std.Option.reify Error.reify)

        let pprint =
          let pp ff (regs, opterr) =
            pprint_logic (fun ff -> function
              | None      -> ()
              | Some err  ->
                Format.fprintf ff "@[<v>Error:%a@]@;" Error.pprint err
            ) ff opterr;
            Format.fprintf ff "@[<v>Regs:@;<1 2>%a@]@."
              RegStorage.pprint regs
          in
          pprint_logic pp

        let regso ?err res rs =
          (res === Std.pair rs (Std.Option.option err))

        let erroro res err =
          fresh (rs)
            (res === Std.pair rs (Std.some err))
      end

    let interpo prog rs res =
      fresh (prog' s s' rs' opterr)
        (s  === State.init prog rs)
        (s' === State.state prog' rs' opterr)
        (res === Std.pair rs' opterr)
        (State.evalo s s')

  end
