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

module Regs =
  struct
    type tt = (Reg.tt, Value.tt) Storage.tt

    type tl = (Reg.tl, Value.tl) Storage.tl
      and inner = (Reg.tl, Value.tl) Storage.inner

    type ti = (Reg.tt, Value.tt, Reg.tl, Value.tl) Storage.ti

    let empty = Storage.empty

    let alloc rs = Storage.allocate (Value.integer 0) @@ List.map Reg.reg rs
    let init pairs =
      Storage.from_assoc @@ List.map (fun (r, v) -> (Reg.reg r, Value.integer v)) pairs

    let from_assoc xs =
      Storage.from_assoc @@ List.map (fun (r, v) -> (Reg.reg r, Value.integer v)) xs

    let reify = Storage.reify (Reg.reify) (Value.reify)

    let pprint =
      Storage.pprint (fun ff (k, v) -> Format.fprintf ff "%s=%s" (Reg.show k) (Value.show v))

    let reado  = Storage.geto
    let writeo = Storage.seto

    let checko t rvs = Storage.checko t @@ List.map (fun (r, v) -> (Reg.reg r, Value.integer v)) rvs
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
        (Regs.reado rs x v);

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

module CProg =
  struct
    type tt = Prog.tt Std.List.ground

    type tl = inner MiniKanren.logic
      and inner = (Prog.tl, tl) Std.list

    type ti = (Prog.tt, Prog.tl) Std.List.groundi

    let reify = Std.List.reify Prog.reify

    let pprint ff x =
      let rec pp ff =
        let open Std in function
        | Cons (t, ts) ->
          Format.fprintf ff "@;<1 4>@[<v>%a]@;|||%a" Prog.pprint t ppl ts
        | Cons (t, Value Nil) ->
          Format.fprintf ff "@;<1 4>@[<v>%a]@;" Prog.pprint t
        | Nil -> ()
        | _   -> assert false
      and ppl ff x = pprint_logic pp ff x
      in
      ppl ff x
  end

let prog p = Std.List.list p

let cprog ps = Std.List.list ps

let thrdnum : CProg.ti -> int =
  fun p -> List.length @@ Std.List.to_list (fun x -> x) @@ Obj.magic p

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

    let erroro ?(sg=fun _ -> success) ?(fg=failure) label = conde
      [ fresh (err)
          (label === error err)
          (sg err)

      ; ?~(fresh (err) (label === error err)) &&& fg

      ]

  end

module ThreadLocalStorage(T : Utils.Logic) =
  struct
    type tt = ThreadID.tt * (ThreadID.tt, T.tt) Storage.tt

    type tl = inner MiniKanren.logic
      and inner = ThreadID.tl * (ThreadID.tl, T.tl) Storage.tl

    type ti = (tt, tl) MiniKanren.injected

    let reify = Std.Pair.reify ThreadID.reify (Storage.reify ThreadID.reify T.reify)

    let pprint =
      let pp ff (_, thrds) =
      Format.fprintf ff "@[<v>";
      Storage.pprint (
        fun ff (tid, thrd) ->
          Format.fprintf ff "Thread #%a:@;<1 2>%a" ThreadID.pprint tid T.pprint thrd
        ) ff thrds;
      Format.fprintf ff "@]";
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
        (Storage.geto thrds tid thrd)

    let seto tls tls' tid thrd =
      fresh (cnt thrds thrds')
        (tls  === Std.pair cnt thrds )
        (tls' === Std.pair cnt thrds')
        (Storage.seto thrds thrds' tid thrd)

    let tidso tls tids =
      fresh (cnt thrds)
        (tls === Std.pair cnt thrds)
        (Storage.keyso thrds tids)

    let foldo g tls acc acc' =
      fresh (cnt thrds)
        (tls === Std.pair cnt thrds)
        (Storage.foldo (fun _ -> g) thrds acc acc')

    let forallo g tls =
      fresh (cnt thrds)
        (tls === Std.pair cnt thrds)
        (Storage.forallo (fun _ thrd -> g thrd) thrds)

  end

module Thread =
  struct
    module T =
      struct
        @type ('prog, 'tid) t =
          { prog : 'prog
          ; pid  : 'tid
          }
        with gmap

        let fmap fa fb x = GT.gmap(t) fa fb x
      end

    type tt = (Prog.tt, ThreadID.tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (Prog.tl, ThreadID.tl) T.t

    type ti = (tt, tl) MiniKanren.injected

    module F = Fmap2(T)

    let thrd prog pid = T.(inj @@ F.distrib {prog; pid})

    let init ?(pid=ThreadID.null) prog = thrd prog pid

    let reify h = F.reify Prog.reify ThreadID.reify h

    let pprint =
      let pp ff = let open T in fun { prog; pid; } ->
        Format.fprintf ff "@[<v>pid: %a@;@[<v>Code:@;<1 4>%a@;@]@;@]"
          ThreadID.pprint pid
          Prog.pprint prog
      in
      pprint_logic pp

    open Std
    open Stmt

    let terminatedo t =
      fresh (pid)
        (t  === thrd (Stmt.skip ()) pid)

    let asserto label rs rs' t t' =
      fresh (prog prog' pid e v stmts)
        (t  === thrd prog  pid)
        (t' === thrd prog' pid)
        (prog === (assertion e) % stmts)
        (rs === rs')
        (Expr.evalo rs e v)
        (conde [
          (Value.not_nullo v) &&& (prog' === stmts  ) &&& (label === Label.empty ());
          (Value.nullo     v) &&& (prog' === skip ()) &&& (label === Label.error (Error.assertion e));
        ])

    let asgno label rs rs' t t' =
      fresh (prog prog' pid r e v)
        (t  === thrd prog  pid)
        (t' === thrd prog' pid)
        (prog === (asgn r e) % prog')
        (label === Label.empty ())
        (Expr.evalo rs e v)
        (Regs.writeo rs rs' r v)

    let ifo label rs rs' t t' =
      fresh (prog prog' pid e v t1 t2 stmts)
        (t  === thrd prog  pid)
        (t' === thrd prog' pid)
        (prog === (if' e t1 t2) % stmts)
        (label === Label.empty ())
        (rs === rs')
        (Expr.evalo rs e v)
        (conde [
          (Value.not_nullo v) &&& (seqo t1 stmts prog');
          (Value.nullo     v) &&& (seqo t2 stmts prog');
        ])

    let whileo label rs rs' t t' =
      fresh (prog prog' pid e body stmt stmt' stmts stmts')
        (t  === thrd prog  pid)
        (t' === thrd prog' pid)
        (prog  === stmt  % stmts)
        (prog' === stmt' % stmts)
        (stmt  === (while' e body))
        (stmt' === if' e stmts' (skip ()))
        (label === Label.empty ())
        (rs === rs')
        (seqo body (single stmt) stmts')

    let repeato label rs rs' t t' =
      fresh (prog prog' pid e body body' stmts)
        (t  === thrd prog  pid)
        (t' === thrd prog' pid)
        (prog  === (repeat body e) % stmts)
        (label === Label.empty ())
        (rs === rs')
        (seqo body (single @@ while' (Expr.unop !!Uop.NOT e) body) body')
        (seqo body' stmts prog')

    let loado label rs rs' t t' =
      fresh (prog prog' pid mo l r v)
        (t  === thrd prog  pid)
        (t' === thrd prog' pid)
        (prog === (load mo l r) % prog')
        (label === Label.load mo l v)
        (Regs.writeo rs rs' r v)

    let storeo label rs rs' t t' =
      fresh (prog prog' pid mo l e v)
        (t  === thrd prog  pid)
        (t' === thrd prog' pid)
        (prog === (store mo l e) % prog')
        (label === Label.store mo l v)
        (rs === rs')
        (Expr.evalo rs e v)

    let dataraceo label rs rs' t t' =
      fresh (prog prog' pid stmt mo l r e)
        (t  === thrd prog  pid)
        (t' === thrd prog' pid)
        (prog === stmt % prog')
        (label === Label.error (Error.datarace mo l))
        (rs === rs')
        ((stmt === load mo l r) ||| (stmt === store mo l e))

    let stepo label rs rs' t t' =
      let rules =
        [asserto; asgno; ifo; whileo; repeato; loado; storeo; dataraceo]
      in
      conde @@ List.map (fun rule -> rule label rs rs' t t') rules

  end

module ThreadManager =
  struct
    include ThreadLocalStorage(Thread)

    (* unsafe cast here, because of OCanren's weird typesystem;
     * probably we need `project` function here,
     * then we can project injected list of programs into regular one
     *)
    let make : CProg.ti -> ti = fun ps ->
      let ps : Prog.ti list = Std.List.to_list (fun x -> x) @@ Obj.magic ps in
      of_list @@ List.map (fun prog -> Thread.init prog) ps
      (* of_list [] *)

    let terminatedo = forallo Thread.terminatedo

    let stepo tid label rs rs' ts ts' =
      fresh (thrd thrd')
        (geto ts tid thrd)
        (seto ts ts' tid thrd')
        (Thread.stepo label rs rs' thrd thrd')

  end

module RegStorage =
  struct
    include ThreadLocalStorage(Regs)
  end

module type MemoryModel =
  sig
    include Utils.Logic

    val alloc : thrdn:int -> string list -> ti
    val init  : thrdn:int -> (string * int) list -> ti

    val stepo : ThreadID.ti -> Label.ti -> ti -> ti -> MiniKanren.goal
  end

module State(Memory : MemoryModel) =
  struct
    module T =
      struct
        @type ('regs, 'mem, 'opterr) t =
          { regs   : 'regs
          ; mem    : 'mem
          ; opterr : 'opterr
          }
        with gmap

        let fmap fa fb fc x = GT.gmap(t) fa fb fc x
      end

    type tt = (RegStorage.tt, Memory.tt, Error.tt Std.Option.ground) T.t

    type tl = inner MiniKanren.logic
      and inner = (RegStorage.tl, Memory.tl, Error.tl Std.Option.logic) T.t

    type ti = (tt, tl) MiniKanren.injected

    module F = Fmap3(T)

    let reify = F.reify
      RegStorage.reify
      Memory.reify
      (Std.Option.reify Error.reify)

    let pprint =
      let pp ff = let open T in fun {regs; mem; opterr} ->
        Format.fprintf ff "@[<v>";
        pprint_logic (fun ff -> function
          | None      -> ()
          | Some err  ->
            Format.fprintf ff "Error: %a@;" Error.pprint err
        ) ff opterr;
        (* Format.fprintf ff "@[<v>Registers:@;<1 2>%a@]@;@[<v>Memory:@;<1 2>%a@]" *)
        Format.fprintf ff "Registers:@;<1 2>%a@;Memory:@;<1 2>%a"
          RegStorage.pprint regs
          Memory.pprint mem;
        Format.fprintf ff "@]"
      in
      pprint_logic pp

    let state regs mem opterr = T.(inj @@ F.distrib {regs; mem; opterr})

    let init regs mem = state regs mem (Std.none ())

    let memo ?err s mem =
      fresh (regs)
        (s === state regs mem (Std.Option.option err))

    let regso ?err s tid rs =
      fresh (regs mem)
        (s === state regs mem (Std.Option.option err))
        (ThreadManager.geto regs tid rs)

    let regstorageo ?err s regs =
      fresh (mem)
        (s === state regs mem (Std.Option.option err))

    let erroro ?(sg=fun _ -> success) ?(fg=failure) s =
      fresh (regs mem opterr err)
        (s === state regs mem opterr)
        (conde
          [ (opterr === Std.some err) &&& (sg err)
          ; (opterr === Std.none () ) &&& fg
          ])

    let safeo s =
      fresh (regs mem)
        (s === state regs mem (Std.none ()))

    let dataraceo s =
      erroro s ~sg:(fun err -> fresh (mo loc) (err === Error.datarace mo loc))

  end

module ProgramState(Memory : MemoryModel) =
  struct
    module State = State(Memory)

    type tt = ThreadManager.tt * State.tt

    type tl = inner MiniKanren.logic
      and inner = ThreadManager.tl * State.tl

    type ti = (tt, tl) MiniKanren.injected

    let reify = Std.Pair.reify ThreadManager.reify State.reify

    let pprint =
      let pp ff (tm, s) =
        Format.fprintf ff "@[<v>Threads:@;<1 2>%a@;State:@;<1 2>%a@]@."
          ThreadManager.pprint tm
          State.pprint s
      in
      pprint_logic pp

    let progstate = Std.pair

    let make p = progstate @@ ThreadManager.make p
  end

type tactic =
  | SingleThread of ThreadID.ti
  | Sequential
  | Interleaving

module ConcurrentInterpreter(Memory : MemoryModel) =
  struct
    module State = State(Memory)
    module ProgramState = ProgramState(Memory)

    open Std
    open State
    open ProgramState

    let terminatedo ?tid t =
      let fk tm = match tid with
      | None      -> ThreadManager.terminatedo tm
      | Some tid  ->
        fresh (thrd)
          (ThreadManager.geto tm tid thrd)
          (Thread.terminatedo thrd)
      in
      fresh (tm s)
        (t === progstate tm s)
        (State.erroro s ~fg:(fk tm))

    let stepo tid t t' =
      fresh (tm tm' s s' regs regs' rs rs' mem mem' opterr label)
        (t  === progstate tm  s )
        (t' === progstate tm' s')
        (s  === state regs  mem  (none ()))
        (s' === state regs' mem' opterr)
        (RegStorage.geto regs       tid rs )
        (RegStorage.seto regs regs' tid rs')
        (ThreadManager.stepo tid label rs rs' tm tm')
        (Memory.stepo tid label mem mem')
        (Label.erroro label
          ~sg:(fun err -> opterr === some err)
          ~fg:(opterr === none ())
        )

    let rec evalo = let open Semantics.Reduction in function
    | SingleThread tid ->
      make_eval ~irreducibleo:(terminatedo ~tid) @@ stepo tid
    | Sequential ->
      fun t t' ->
        fresh (tm s tids)
          (t === progstate tm s)
          (ThreadManager.tidso tm tids)
          (Utils.foldlo tids ~init:t ~res:t'
            ~g:(fun tid t t' -> conde
              [ (evalo (SingleThread tid) t t')
              ; fresh (tm s)
                  (t === progstate tm s)
                  (State.erroro s ~sg:(fun _ -> t === t'))
              ])
          )
    | Interleaving ->
      let stepo t t' =
        fresh (tid)
          (stepo tid t t')
      in
      make_eval ~irreducibleo:terminatedo stepo

    let interpo tactic prog s s' =
      fresh (t t' tm')
        (t  === ProgramState.make prog s)
        (t' === progstate tm' s')
        (evalo tactic t t')

  end

module SequentialInterpreter =
  struct
    module DummyMM =
      struct
        type tt = unit
        type tl = inner MiniKanren.logic
          and inner = unit
        type ti = (tt, tl) MiniKanren.injected

        let reify = MiniKanren.reify

        let pprint ff t = ()

        let instance () = !!()

        let alloc ~thrdn locs = instance ()
        let init  ~thrdn mem  = instance ()

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

        let init rs = init (RegStorage.of_list [rs]) (DummyMM.instance ())

        let regso ?err t rs = regso ?err t ThreadID.init rs
      end

    module ProgramState =
      struct
        include ProgramState(DummyMM)

        let make p = progstate @@ ThreadManager.make (cprog [p])
      end

    module Helper = ConcurrentInterpreter(DummyMM)

    let evalo = Helper.evalo (SingleThread ThreadID.init)

    let interpo prog s s' =
      fresh (t t' tm')
        (t  === ProgramState.make prog s)
        (t' === ProgramState.progstate tm' s')
        (evalo t t')
  end
