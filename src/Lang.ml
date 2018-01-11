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

    let fst  = Std.Nat.zero
    let succ = Std.Nat.succ

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

    let empty = MiniKanren.Std.nil

    let allocate = Storage.allocate (Value.integer 0)

    let from_assoc = Storage.from_assoc

    let reify = Storage.reify (Reg.reify) (Value.reify)

    let pprint =
      Storage.pprint (fun ff (k, v) -> Format.fprintf ff "%s=%s" (Reg.show k) (Value.show v))

    let reado  = Storage.geto
    let writeo = Storage.seto
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

    let pprint ff x = T.(
      let rec pp ff = function
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
      and ppseq ff =
        let open Std in function
        | Cons (t, ts) -> Format.fprintf ff "@[<v>%a;@;%a@]" ppl t ppseql ts
        | Nil          -> ()
      and ppl = pprint_logic pp
      and ppseql = pprint_logic ppseq
      in
      Format.fprintf ff "%a@." ppl x
    )

  end

module Label =
  struct
    module T =
      struct
        @type ('tid, 'tids, 'mo, 'loc, 'value) t =
          | Empty
          | Spawn           of 'tid * 'tids
          | Join            of 'tids
          | Load            of 'mo * 'loc * 'value
          | Store           of 'mo * 'loc * 'value
          | CAS             of 'mo * 'mo * 'loc * 'value * 'value * 'value
          | Datarace        of 'mo * 'loc
          | AssertionFailed
        with gmap, show

        let fmap fa fb fc fd fe x = GT.gmap(t) fa fb fc fd fe x
      end

    type tt = (ThreadID.tt, ThreadID.tt Std.List.ground, MemOrder.tt, Loc.tt, Value.tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (ThreadID.tl, ThreadID.tl Std.List.logic, MemOrder.tl, Loc.tl, Value.tl) T.t

    type ti = (tt, tl) MiniKanren.injected

    module FT = Fmap5(T)

    let empty ()               = inj @@ FT.distrib @@ T.Empty
    let spawn pid tids         = inj @@ FT.distrib @@ T.Spawn (pid, tids)
    let join tids              = inj @@ FT.distrib @@ T.Join tids
    let load  mo loc v         = inj @@ FT.distrib @@ T.Load  (mo, loc, v)
    let store mo loc v         = inj @@ FT.distrib @@ T.Store (mo, loc, v)
    let cas mo1 mo2 loc e d v  = inj @@ FT.distrib @@ T.CAS (mo1, mo2, loc, e, d, v)
    let datarace mo loc        = inj @@ FT.distrib @@ T.Datarace (mo, loc)
    let assert_fail ()         = inj @@ FT.distrib @@ T.AssertionFailed

    let reify h = FT.reify ThreadID.reify (Std.List.reify ThreadID.reify) MemOrder.reify Loc.reify Value.reify h

    let show = GT.show(T.t) ThreadID.show (GT.show(Std.List.logic) ThreadID.show) MemOrder.show Loc.show Value.show

    let pprint = pprint_logic @@ (fun ff x -> Format.fprintf ff "@[%s@]" (show x))

  end

module Rules =
  struct
    open Stmt

    let asserto label rs rs' t ts =
      fresh (e v h)
        (t  === assertion e)
        (ts === skip ())
        (rs === rs')
        (Expr.evalo rs e v)
        (conde [
          (Value.nullo v)     &&& (label === Label.assert_fail ());
          (Value.not_nullo v) &&& (label === Label.empty ());
        ])

    let asgno label rs rs' t ts =
      fresh (r e v)
        (t  === asgn r e)
        (ts === skip ())
        (label === Label.empty ())
        (Expr.evalo rs e v)
        (RegStorage.writeo rs rs' r v)

    let ifo label rs rs' t ts =
      fresh (e v t1 t2)
        (t === if' e t1 t2)
        (rs === rs')
        (label === Label.empty ())
        (Expr.evalo rs e v)
        (conde [
          (Value.not_nullo v) &&& (ts === t1);
          (Value.nullo v)     &&& (ts === t2);
        ])

    let whileo label rs rs' t ts =
      fresh (e body t')
        (t  === while' e body)
        (ts === single @@ if' e t' (skip ()))
        (rs === rs')
        (label === Label.empty ())
        (seqo body (single t) t')

    let repeato label rs rs' t ts =
      fresh (e body)
        (t  === repeat body e)
        (rs === rs')
        (label === Label.empty ())
        (seqo body (single @@ while' (Expr.unop !!Uop.NOT e) body) ts)

    let loado label rs rs' t ts =
      fresh (mo l r v)
        (t  === load mo l r)
        (ts === skip ())
        (label === Label.load mo l v)
        (RegStorage.writeo rs rs' r v)

    let storeo label rs rs' t ts =
      fresh (mo l e v)
        (t  === store mo l e)
        (ts === skip ())
        (rs === rs')
        (label === Label.store mo l v)
        (Expr.evalo rs e v)

    let dataraceo label rs rs' t ts =
      fresh (mo l r e h)
        ((t === load mo l r) ||| (t === store mo l e))
        (ts === skip ())
        (label === Label.datarace mo l)

    let all = [asserto; asgno; ifo; whileo; repeato; loado; storeo; dataraceo]
  end

module Thread =
  struct
    module T =
      struct
        @type ('regs, 'prog, 'tid, 'tids) t =
          { regs : 'regs
          ; prog : 'prog
          ; pid  : 'tid
          ; wait : 'tids
          }
        with gmap

        let fmap fa fb fc fd x = GT.gmap(t) fa fb fc fd x
      end

    type tt = (RegStorage.tt, Stmt.tt, ThreadID.tt, ThreadID.tt Std.List.ground) T.t

    type tl = inner MiniKanren.logic
      and inner = (RegStorage.tl, Stmt.tl, ThreadID.tl, ThreadID.tl Std.List.logic) T.t

    type ti = (tt, tl) MiniKanren.injected

    module F = Fmap4(T)

    let thrd prog regs pid wait = T.(inj @@ F.distrib {prog; regs; pid; wait})

    let reify h = F.reify RegStorage.reify Stmt.reify ThreadID.reify (Std.List.reify ThreadID.reify) h

    open Stmt
    open Std

    let setrego t t' r v =
      fresh (prog prog' rs rs' pid wait)
        (t  === thrd prog rs  pid wait)
        (t' === thrd prog rs' pid wait)
        (RegStorage.writeo rs rs' r v)

    let terminatedo t =
      fresh (rs pid)
        (t  === thrd (skip ()) rs pid (nil ()))

    let stepo label t t' =
      fresh (prog prog' rs rs' pid stmt stmts' stmts)
        (t  === thrd prog  rs  pid (nil ()))
        (t' === thrd prog' rs' pid (nil ()))
        (prog === stmt % stmts)
        (conde @@ List.map (fun rule -> rule label rs rs' stmt stmts') Rules.all)
        (seqo stmts' stmts prog')

    let returno label t t' pid regs vs =
      let rec helpero rs regs vs = conde [
        (regs === nil ()) &&& (vs === nil ());

        fresh (r v regs' vs')
          (regs === r % regs')
          (vs   === v % vs')
          (RegStorage.reado rs r v)
          (helpero rs regs' vs')
      ] in
      fresh (prog prog' rs rs' stmts)
        (t  === thrd prog  rs  pid (nil ()))
        (t' === thrd prog' rs' pid (nil ()))
        (prog  === (Stmt.return regs) % stmts)
        (prog' === nil ())
        (label === Label.empty ())
        (helpero rs regs vs)

    let spawno label t t' tid tids ts =
      let rec helpero pid ps ts = conde [
        (ps === nil ()) &&& (ts === nil ());

        fresh (p t ps' ts')
          (ps === p % ps')
          (ts === t % ts')
          (t === thrd p (RegStorage.empty ()) pid (nil ()))
          (helpero pid ps' ts');
      ] in
      fresh (prog prog' rs pid ps)
        (t  === thrd prog  rs pid (nil ()))
        (t' === thrd prog' rs pid tids)
        (prog  === (Stmt.spw ps) % prog')
        (label === Label.spawn tids)
        (helpero tid ps ts)

    let joino label t t' tids =
      fresh (pid prog rs tids stmts)
        (t  === trhd prog rs pid tids)
        (t' === thrd prog rs pid (nil ()))
        (prog  === (Stmt.join tids) % prog')
        (label === Label.join tids)

  end

module ThreadSubSys =
  struct
    type tt = (ThreadID.tt, Thread.tt) Storage.tt

    type tl = (ThreadID.tl, Thread.tl) Storage.tl
      and inner = (ThreadID.tl, Thread.tl) Storage.inner

    type ti = (ThreadID.tt, Thread.tt, ThreadID.tl, Thread.tl) Storage.ti

    let rec updateo t t' rs vs = conde [
      (rs === nil ()) &&& (vs === ()) &&& (t === t');

      fresh (r v rs' vs' t'')
        (rs === r % rs')
        (vs === v % vs')
        (Thread.setrego t t'' r v)
        (updateo t'' t' rs' vs')
    ]

    let rec extendo t t'' tids thrds = conde [
      (tids === nil ()) &&& (thrds === nil ()) &&& (t === t'');

      fresh (t' tid ts ts' tids' thrds')
        (t  === thrdsys tid ts)
        (t' === thrdsys (ThreadID.succ tid) ts')
        (tids === tid % tids')
        (thrds === thrd % thrds')
        (Storage.extendo ts ts' tid thrd)
        (extendo t' t'' tids' thrds')
    ]

    let rec removeo t t' tids = conde [
      (tids === nil ()) &&& (t === t');

      fresh (tids' t'')
        (tids === tid % tids')
        (Storage.geto t tid thrd)
        (Thread.terminatedo thrd)
        (Storage.removeo t t'' tid)
        (removeo t'' t' tids')
    ]

    let step tid label t t' =
      fresh (thrd thrd')
        (Storage.membero t tid thrd)
        (conde [
          (Thread.skipo label thrd thrd');

          (Thread.stepo label thrd thrd');

          fresh (rs vs pthrd pthrd')
            (Thread.returno label thrd thrd' pid rs vs)
            (Storage.geto t    pid pthrd )
            (Storage.seto t t' pid pthrd')
            (updateo pthrd pthrd' rs vs);

          fresh (tids ts t'')
            (Thread.spawno label thrd thrd' tid tids ts)
            (Storage.seto t t'' tid thrd')
            (extendo t'' t' tids ts);

          fresh (tids t'')
            (Thread.joino label thrd thrd' tids)
            (Storage.seto t t'' tid thrd')
            (removeo t'' t' tids);
        ])

  end
