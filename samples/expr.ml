open MiniKanren
open MiniKanrenStd
open Printf

module Expr =
  struct
    module T =
      struct
        @type ('var, 'value, 't) t =
          | Const   of 'value
          | Var     of 'var
          | Add     of 't * 't
          | Eq      of 't * 't
          | Lt      of 't * 't
          | If      of 't * 't * 't
        with gmap, show

        let fmap fa fb x = GT.gmap(t) fa fb x
      end

    type tt = (string, Nat.ground, tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (string MiniKanren.logic, Nat.logic, tl) T.t

    type ti = (tt, tl) MiniKanren.injected

    module F = Fmap2(T)

    let const v     = inj @@ F.distrib @@ T.Const v
    let var x       = inj @@ F.distrib @@ T.Var x
    let add l r     = inj @@ F.distrib @@ T.Add (l, r)
    let eq  l r     = inj @@ F.distrib @@ T.Eq  (l, r)
    let lt  l r     = inj @@ F.distrib @@ T.Lt  (l, r)
    let if' c l r   = inj @@ F.distrib @@ T.If  (c, l, r)

    let one  () = const Nat.one
    let zero () = const Nat.zero

    let of_int i =
      let rec helper i =
        if i = 0 then Nat.zero else Nat.succ @@ helper (i-1)
      in
      const @@ helper i

    let rec reify h = F.reify (MiniKanren.reify) (Nat.reify) reify h

    let rec show x = T.(
      let rec show_nat n =
        let rec to_ground = function
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
            let cs = String.concat "; " @@ List.map show_nat cs in
            Printf.sprintf "_.%d{=/= %s}" i cs
      in
      let show_var = GT.show(logic) (fun s -> s) in
      let helper : inner -> string = function
      | Const v       -> show_nat v
      | Var x         -> show_var x
      | Add (l, r)    -> sprintf "%s + %s" (show l) (show r)
      | Eq  (l, r)    -> sprintf "%s = %s" (show l) (show r)
      | Lt  (l, r)    -> sprintf "%s < %s" (show l) (show r)
      | If (c, l, r)  -> sprintf "if %s then %s else %s" (show c) (show l) (show r)
      in
      GT.show(logic) helper x
    )

    let pprint ff r = Format.fprintf ff "%s@." @@ show r

    let lookupo s x v = List.membero s @@ Pair.pair x v

    let rec evalo s e v = Nat.(conde
      [ fresh (v)
          (e === const v)

      ; fresh (x)
          (e === var x)
          (lookupo s x v)

      ; fresh (l r x y)
          (e === add l r)
          (addo x y v)
          (evalo s l x)
          (evalo s r y)

      ; fresh (l r x y z)
          (e === eq l r)
          (conde
          [ (x === y) &&& (v === Nat.one )
          ; (x =/= y) &&& (v === Nat.zero)
          ])
          (evalo s l x)
          (evalo s r y)

      ; fresh (l r x y)
          (e === lt l r)
          (evalo s l x)
          (evalo s r y)
          (conde
          [ (lto x y Bool.truo ) &&& (v === Nat.one )
          ; (lto x y Bool.falso) &&& (v === Nat.zero)
          ])

      ; fresh (c l r x)
          (e === if' c l r)
          (evalo s c x)
          (conde
          [ (x === one ) &&& (evalo l v)
          ; (x === zero) &&& (evalo r v)
          ])
      ])

      val intrpo prog input output = evalo input prog output
  end

let emax = Expr.(
  let x = var !!"x" in
  let y = var !!"y" in
  if' (lt x y) y x
)

let input v1 v2 =
  let inj_pair (x, v) = Pair.pair x v in
  MiniKanren.list inj_pair [(!!"x", v1); (!!"y", v2)]

let show_inut =
  | [("x", x); ("y", y)] -> sprintf "x=%d, y=%d" (Nat.to_int x) (Nat.to_int y)
  | _ -> invalid_arg ""

let reifyq  qs    = Stream.map (fun rr -> rr#reify Expr.reify) qs
let reifyqr qs rs = Stream.zip (reifyq qs) (reifyq rs)

let nati n = Nat.(nat @@ of_int n)

let () = Expr.(Nat.(
  let reify = List.reify (Pair.reify reify Nat.reify) in
  let inputo i =
    fresh (v1 v2)
      (i === input v1 v2)
      (v1 < nati 16)
      (v2 < nati 16)
      (v1 < v2)
  in
  let asserto i o =
    fresh (v1 v2)
      (i === input v1 v2)
      (v2 === o)
  in
  let stream = Query.verify Expr.intrpo inputo asserto emax in
  if Stream.is_empty stream then
    printf "Success!\n"
  else
    let [cex] = Stream.take ~n:1 stream in
    printf "Fail!\nCounterexample: %s\n" (show_input @@ cex#reify reify)
))

(* let () = Expr.(
  let x, y = one (), zero () in
  let answs = run q (fun z -> evalo (emax x y) z) reifyq in
  let [z] = Stream.take answs in
  printf "max(1, 0) = %s\n" (show z)
)

let () = Expr.(Nat.(
  let answs = run qr (fun x y ->
    fresh (z x' y' z')
      (x === const x')
      (y === const y')
      (z === const z')
      (x' < nati 16)
      (y' < nati 16)
      (x' < y')
      (z' =/= y')
      (evalo (emax x y) z)
    )
    reifyqr
  in
  if Stream.is_empty answs then
    printf "success\n"
  else
    let [(x, y)] = Stream.take ~n:1 answs in
    printf "fail\ncounterexample: %s\n" (show_input x) (show y)
)) *)

(* let () = Expr.(Nat.(
  let answs = run q (fun p ->
    fresh (z)
      (x === const x')
      (y === const y')
      (z === const z')
      (x' < nati 16)
      (y' < nati 16)
      (x' < y')
      (z' =/= y')
      (evalo (emax x y) )
    )
    reifyq
  in
  if Stream.is_empty answs then
    printf "fail\n"
  else
    let [p] = Stream.take ~n:1 answs in
    printf "success: %s\n" (show p)
)) *)

(* let () = Expr.(
  let qs = run q (fun q -> evalo (emax (one ()) (zero ())) q) (fun qs -> qs) in
  List.iter (pprint Format.std_formatter) @@ Stream.take qs
) *)
