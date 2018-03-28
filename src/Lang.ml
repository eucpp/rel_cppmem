(* Copyright (c) 2016-2018
 * Evgenii Moiseenko and Anton Podkopaev
 * St.Petersburg State University, JetBrains Research
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

open MiniKanren
open Utils

module Reg =
  struct
    type tt = string

    type tl = inner MiniKanren.logic
      and inner = string

    type ti = (tt, tl) MiniKanren.injected
    type ri = (tt, tl) MiniKanren.reified

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
    type ri = (tt, tl) MiniKanren.reified

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
    type ri = (tt, tl) MiniKanren.reified

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

    let subo x y z = addo y z x

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
    type ri = (tt, tl) MiniKanren.reified

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
    type ri = (Reg.tt, Value.tt, Reg.tl, Value.tl) Storage.ri

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
    type ri = (tt, tl) MiniKanren.reified

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
    type tt = ADD | SUB | MUL | EQ | NEQ | LT | LE | GT | GE | OR | AND

    type tl = tt MiniKanren.logic

    type ti = (tt, tl) MiniKanren.injected
    type ri = (tt, tl) MiniKanren.reified

    let of_string = function
      | "+"   -> ADD
      | "-"   -> SUB
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
      | SUB   -> "-"
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
    type ri = (tt, tl) MiniKanren.reified

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
          | Choice  of 't * 't
        with gmap, show

        let fmap fa fb fc fd fe x = GT.gmap(t) fa fb fc fd fe x
      end

    type tt = (Reg.tt, Value.tt, Uop.tt, Bop.tt, tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (Reg.tl, Value.tl, Uop.tl, Bop.tl, tl) T.t

    type ti = (tt, tl) MiniKanren.injected
    type ri = (tt, tl) MiniKanren.reified

    module FT = Fmap5(T)

    let var x         = inj @@ FT.distrib @@ T.Var x
    let const v       = inj @@ FT.distrib @@ T.Const v
    let unop op e     = inj @@ FT.distrib @@ T.Unop (op, e)
    let binop op l r  = inj @@ FT.distrib @@ T.Binop (op, l, r)
    let choice l r    = inj @@ FT.distrib @@ T.Choice (l, r)

    let rec reify h = FT.reify (Reg.reify) (Value.reify) (Uop.reify) (Bop.reify) reify h

    let rec show t =
      GT.show(logic) (GT.show(T.t) Reg.show Value.show Uop.show Bop.show show) t

    let rec pprint ff x = T.(
      let s ff = function
        | Var x            -> Format.fprintf ff "@[%a@]" Reg.pprint x
        | Const n          -> Format.fprintf ff "@[%a@]" Value.pprint n
        | Unop (op, e)     -> Format.fprintf ff "@[%a (%a)@]" Uop.pprint op pprint e
        | Binop (op, l, r) -> Format.fprintf ff "@[%a %a %a@]" pprint l Bop.pprint op pprint r
        | Choice (l, r)    -> Format.fprintf ff "@[choice(%a, %a)@]" pprint l pprint r
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
          (op === !!Bop.SUB) &&& (subo x y v);
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
        ]);

      fresh (l r)
        (e === choice l r)
        (conde
          [ (evalo rs l v)
          ; (evalo rs r v)
          ]
        )

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
          | Cas      of 'mo * 'mo * 'loc * 'e * 'e * 'reg
          | Spw      of 'ts * 'ts
          | Return   of 'regs
        with gmap, show

        let fmap fa fb fc fd fe ff x = GT.gmap(t) fa fb fc fd fe ff x
      end

    type tt = (Reg.tt, Reg.tt Std.List.ground, Loc.tt, MemOrder.tt, Expr.tt, tt Std.List.ground) T.t

    type tl = inner MiniKanren.logic
      and inner = (Reg.tl, Reg.tl Std.List.logic, Loc.tl, MemOrder.tl, Expr.tl, tl Std.List.logic) T.t

    type ti = (tt, tl) MiniKanren.injected
    type ri = (tt, tl) MiniKanren.reified

    module FT = Fmap6(T)

    let assertion e           = inj @@ FT.distrib @@ T.Assert e
    let asgn r e              = inj @@ FT.distrib @@ T.Asgn (r, e)
    let if' e l r             = inj @@ FT.distrib @@ T.If (e, l, r)
    let while' e t            = inj @@ FT.distrib @@ T.While (e, t)
    let repeat t e            = inj @@ FT.distrib @@ T.Repeat (t, e)
    let load mo l r           = inj @@ FT.distrib @@ T.Load (mo, l, r)
    let store mo l t          = inj @@ FT.distrib @@ T.Store (mo, l, t)
    let cas mo1 mo2 l e1 e2 r = inj @@ FT.distrib @@ T.Cas (mo1, mo2, l, e1, e2, r)
    let spw t1 t2             = inj @@ FT.distrib @@ T.Spw (t1, t2)
    let return rs             = inj @@ FT.distrib @@ T.Return rs

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
    and ppseq ff xs =
      let open Std in
      Format.fprintf ff "@[<v>";
      begin match xs with
      | Cons (t, ts) ->
        Format.fprintf ff "%a;@;%a" ppl t ppseql ts
      | Cons (t, Value Nil) ->
        Format.fprintf ff "%a@;" ppl t
      | Nil -> ()
      | _   -> assert false
      end;
      Format.fprintf ff "@]@."
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
      | Cas (m1, m2, l, e, d, r)   ->
        Format.fprintf ff "@[%a := cas_%a_%a(%a, %a, %a)@]" Reg.pprint r MemOrder.pprint m1 MemOrder.pprint m2 Loc.pprint l Expr.pprint e Expr.pprint d
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
    type ri = (tt, tl) MiniKanren.reified

    let reify = Std.List.reify Stmt.reify

    let pprint = Stmt.ppseql
  end

module CProg =
  struct
    type tt = Prog.tt Std.List.ground

    type tl = inner MiniKanren.logic
      and inner = (Prog.tl, tl) Std.list

    type ti = (Prog.tt, Prog.tl) Std.List.groundi
    type ri = (tt, tl) MiniKanren.reified

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

    type ti = (tt, tl) MiniKanren.injected
    type ri = (tt, tl) MiniKanren.reified

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
    type ri = (tt, tl) MiniKanren.reified

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

module Prop =
  struct
    module T =
      struct
        @type ('tid, 'reg, 'loc, 'value, 't) t =
          | True
          | False
          | RegEq       of 'tid * 'reg * 'value
          | LocEq       of 'loc * 'value
          | Conj        of 't * 't
          | Disj        of 't * 't
          | Neg         of 't
          | Datarace
          | Assertion
        with gmap, show

        let fmap fa fb fc fd fe x = GT.gmap(t) fa fb fc fd fe x
      end

    type tt = (ThreadID.tt, Reg.tt, Loc.tt, Value.tt, tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (ThreadID.tl, Reg.tl, Loc.tl, Value.tl, tl) T.t

    type ti = (tt, tl) MiniKanren.injected
    type ri = (tt, tl) MiniKanren.reified

    module F = Fmap5(T)

    let true_ ()          = inj @@ F.distrib @@ T.True
    let false_ ()         = inj @@ F.distrib @@ T.False
    let reg_eq tid reg v  = inj @@ F.distrib @@ T.RegEq (tid, reg, v)
    let loc_eq loc v      = inj @@ F.distrib @@ T.LocEq (loc, v)
    let conj p1 p2        = inj @@ F.distrib @@ T.Conj (p1, p2)
    let disj p1 p2        = inj @@ F.distrib @@ T.Disj (p1, p2)
    let neg p             = inj @@ F.distrib @@ T.Neg p
    let datarace ()       = inj @@ F.distrib @@ T.Datarace
    let assertion ()      = inj @@ F.distrib @@ T.Assertion

    let rec reify h = F.reify ThreadID.reify Reg.reify Loc.reify Value.reify reify h

    let pprint =
      let rec pp ff = T.(function
        | True ->
          Format.fprintf ff "@[true@]"
        | False ->
          Format.fprintf ff "@[false@]"
        | RegEq (tid, r, v) ->
          Format.fprintf ff "@[%a#%a = %a@]" ThreadID.pprint tid Reg.pprint r Value.pprint v
        | LocEq (l, v) ->
          Format.fprintf ff "@[%a = %a@]" Loc.pprint l Value.pprint v
        | Conj (p1, p2) ->
          Format.fprintf ff "@[(%a) && (%a)@]" ppl p1 ppl p2
        | Disj (p1, p2) ->
          Format.fprintf ff "@[(%a) || (%a)@]" ppl p1 ppl p2
        | Neg p ->
          Format.fprintf ff "@[not (%a)@]" ppl p
        | Datarace ->
          Format.fprintf ff "@[datarace@]"
        | Assertion ->
          Format.fprintf ff "@[assertion@]"
      )
      and ppl ff x = pprint_logic pp ff x
      in
      ppl

    type lhs = TidReg of ThreadID.ti * Reg.ti | Loc of Loc.ti

    let (%) tid r = TidReg (ThreadID.tid tid, Reg.reg r)

    let (~) l = Loc (Loc.loc l)

    let (=) lhs v =
      match lhs with
      | TidReg (tid, reg) -> reg_eq tid reg (Value.integer v)
      | Loc loc           -> loc_eq loc (Value.integer v)

    let (&&) = conj
    let (||) = disj
    let (!)  = neg

  end

module ThreadLocalStorage(T : Utils.Logic) =
  struct
    type tt = ThreadID.tt * (ThreadID.tt, T.tt) Storage.tt

    type tl = inner MiniKanren.logic
      and inner = ThreadID.tl * (ThreadID.tl, T.tl) Storage.tl

    type ti = (tt, tl) MiniKanren.injected
    type ri = (tt, tl) MiniKanren.reified

    let reify = Std.Pair.reify ThreadID.reify (Storage.reify ThreadID.reify T.reify)

    let pprint =
      let pp ff (_, thrds) =
      Format.fprintf ff "@[<v>";
      Storage.pprint (
        fun ff (tid, thrd) ->
          Format.fprintf ff "Thread #%a:@;<1 4>%a@;" ThreadID.pprint tid T.pprint thrd
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
        if i > n then xs else helper (i+1) (thrd::xs)
      in
      of_list @@ List.rev @@ helper 1 []

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
        @type ('prog, 'regs, 'tid) t =
          { prog : 'prog
          ; regs : 'regs
          ; pid  : 'tid
          }
        with gmap

        let fmap fa fb fc x = GT.gmap(t) fa fb fc x
      end

    type tt = (Prog.tt, Regs.tt, ThreadID.tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (Prog.tl, Regs.tl, ThreadID.tl) T.t

    type ti = (tt, tl) MiniKanren.injected
    type ri = (tt, tl) MiniKanren.reified

    module F = Fmap3(T)

    let thrd prog regs pid = T.(inj @@ F.distrib {prog; regs; pid})

    let init ?(pid=ThreadID.null) prog rs = thrd prog rs pid

    let reify h = F.reify Prog.reify Regs.reify ThreadID.reify h

    let pprint =
      let pp ff = let open T in fun { prog; regs; pid; } ->
        (* Format.fprintf ff "@[<v>pid: %a@;@]@;@[<v>Regs:@;<1 4>%a@]@;@[<v>Code:@;<1 4>%a@]@." *)
        Format.fprintf ff "@[<v>pid: %a@;Regs:@;<1 4>%a@;Code:@;<1 4>%a@]"
          ThreadID.pprint pid
          Regs.pprint regs
          Prog.pprint prog
      in
      pprint_logic pp

    open Std
    open Stmt

    let regso t rs =
      fresh (pid p)
        (t  === thrd p rs pid)

    let terminatedo t =
      fresh (pid regs)
        (t  === thrd (Stmt.skip ()) regs pid)

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
        (Regs.writeo rs rs' r v)

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
        (Regs.writeo rs rs' r v)

    let storeo label t t' =
      fresh (prog prog' rs pid mo l e v)
        (t  === thrd prog  rs pid)
        (t' === thrd prog' rs pid)
        (prog === (store mo l e) % prog')
        (label === Label.store mo l v)
        (Expr.evalo rs e v)

    let caso label t t' =
      fresh (prog prog' rs rs' pid mo1 mo2 l r exp expv des desv v)
        (t  === thrd prog  rs  pid)
        (t' === thrd prog' rs' pid)
        (prog === (cas mo1 mo2 l exp des r) % prog')
        (label === Label.cas mo1 mo2 l expv desv v)
        (Expr.evalo rs exp expv)
        (Expr.evalo rs des desv)
        (Regs.writeo rs rs' r v)

    let dataraceo label t t' =
      fresh (prog prog' rs pid stmt mo l r e)
        (t  === thrd prog  rs pid)
        (t' === thrd prog' rs pid)
        (prog === stmt % prog')
        (label === Label.error (Error.datarace mo l))
        ((stmt === load mo l r) ||| (stmt === store mo l e))

    let stepo label t t' =
      let rules =
        [asserto; asgno; ifo; whileo; repeato; loado; storeo; caso; dataraceo]
      in
      conde @@ List.map (fun rule -> rule label t t') rules

  end

module ThreadManager =
  struct
    include ThreadLocalStorage(Thread)

    let init ps rs =
      List.combine ps rs
      |> List.map (fun (prog, regs) -> Thread.init prog regs)
      |> of_list

    let terminatedo = forallo Thread.terminatedo

    let stepo tid label ts ts' =
      fresh (thrd thrd')
        (geto ts tid thrd)
        (seto ts ts' tid thrd')
        (Thread.stepo label thrd thrd')

    let rec non_silent_stepo tid label ts ts'' =
      fresh (ts' label')
        (stepo tid label' ts ts')
        (conde
          [ (label' =/= Label.empty ()) &&& (label === label') &&& (ts' === ts'')
          ; (label' === Label.empty ()) &&& (non_silent_stepo tid label ts' ts'')
          ]
        )
  end

module SeqProg =
  struct
    module Result =
      struct
        module T =
          struct
            @type ('regs, 'opterr) t =
              { regs    : 'regs
              ; opterr  : 'opterr
              }
            with gmap

            let fmap fa fb x = GT.gmap(t) fa fb x
          end

        type tt = (Regs.tt, Error.tt Std.Option.ground) T.t

        type tl = inner MiniKanren.logic
          and inner = (Regs.tl, Error.tl Std.Option.logic) T.t

        type ti = (tt, tl) MiniKanren.injected
        type ri = (tt, tl) MiniKanren.reified

        module F = Fmap2(T)

        let result regs opterr = T.(inj @@ F.distrib {regs; opterr})

        let reify h = F.reify Regs.reify (Std.Option.reify Error.reify) h

        let pprint =
          let pp ff = let open T in fun {regs; opterr} ->
            Format.fprintf ff "@[<v>";
            pprint_logic (fun ff -> function
              | None      -> ()
              | Some err  ->
                Format.fprintf ff "Error: %a@;" Error.pprint err
            ) ff opterr;
            Format.fprintf ff "Registers:@;<1 2>%a" Regs.pprint regs;
            Format.fprintf ff "@]@;"
          in
          pprint_logic pp

        let regso t rs =
          fresh (opterr)
            (t === result rs opterr)

        let erroro ?(sg=fun _ -> success) ?(fg=failure) t =
          fresh (rs opterr)
            (t === result rs opterr)
            (conde
              [ fresh (err)
                  (opterr === Std.Option.some err)
                  (sg err)
              ; (opterr === Std.Option.none ()) &&& fg
              ])

      end

    let evalo p rs res =
      let evalo_norec evalo_rec t t'' opterr = conde
        [ (ThreadManager.terminatedo t) &&& (opterr === Std.none ()) &&& (t === t'')
        ; fresh (label t')
            (ThreadManager.stepo (ThreadID.tid 1) label t t')
            (Label.erroro label
              ~sg:(fun err -> (opterr === Std.some err) &&& (t === t''))
              ~fg:(evalo_rec t' t'' opterr)
            )
        ]
      in
      let evalo = Tabling.(tabledrec three evalo_norec) in
      fresh (thrd thrd' tm tm' rs' opterr)
        (tm   === ThreadManager.of_list [thrd ])
        (tm'  === ThreadManager.of_list [thrd'])
        (thrd === Thread.init p rs)
        (res  === Result.result rs' opterr)
        (Thread.regso thrd' rs')
        (evalo tm tm' opterr)
  end
