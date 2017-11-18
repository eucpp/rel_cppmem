open MiniKanren
open MiniKanrenStd
open Printf

module Expr =
  struct
    module T =
      struct
        @type ('value, 't) t =
          | Const of 'value
          | Add   of 't * 't
          | Eq    of 't * 't
          | Lt    of 't * 't
          | If    of 't * 't * 't
        with gmap, show

        let fmap fa fb x = GT.gmap(t) fa fb x
      end

    type tt = (Nat.ground, tt) T.t

    type tl = inner MiniKanren.logic
      and inner = (Nat.logic, tl) T.t

    type ti = (tt, tl) MiniKanren.injected

    module F = Fmap2(T)

    let const v   = inj @@ F.distrib @@ T.Const v
    let add l r   = inj @@ F.distrib @@ T.Add (l, r)
    let eq  l r   = inj @@ F.distrib @@ T.Eq  (l, r)
    let lt  l r   = inj @@ F.distrib @@ T.Lt  (l, r)
    let if' c l r = inj @@ F.distrib @@ T.If  (c, l, r)

    let one  () = const Nat.one
    let zero () = const Nat.zero

    let rec reify h = F.reify (Nat.reify) reify h

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
      let helper : inner -> string = function
      | Const v       -> show_nat v
      | Add (l, r)    -> sprintf "%s + %s" (show l) (show r)
      | Eq  (l, r)    -> sprintf "%s = %s" (show l) (show r)
      | Lt  (l, r)    -> sprintf "%s < %s" (show l) (show r)
      | If (c, l, r)  -> sprintf "if %s then %s else %s" (show c) (show l) (show r)
      in
      GT.show(logic) helper x
    )

    let pprint ff r = Format.fprintf ff "%s@." @@ show r

    let rec evalo e e' = Nat.(conde
      [ fresh (v)
          (e  === const v)
          (e' === e)

      ; fresh (l r x y z)
          (e  === add l r)
          (e' === const z)
          (addo x y z)
          (evalo l (const x))
          (evalo r (const y))

      ; fresh (l r x y z)
          (e  === eq l r)
          (e' === const z)
          (conde
          [ (x === y) &&& (z === one )
          ; (x =/= y) &&& (z === zero)
          ])
          (evalo l (const x))
          (evalo r (const y))

      ; fresh (l r x y z)
          (e  === lt l r)
          (e' === const z)
          (evalo l (const x))
          (evalo r (const y))
          (conde
          [ (lto x y Bool.truo ) &&& (z === one )
          ; (lto x y Bool.falso) &&& (z === zero)
          ])

      ; fresh (c l r x)
          (e === if' c l r)
          (evalo c (const x))
          (conde
          [ (x === one ) &&& (evalo l e')
          ; (x === zero) &&& (evalo r e')
          ])
      ])
  end

let emax x y = Expr.(
  if' (lt x y) y x
)

let reifys = Stream.map (fun rr -> rr#reify Expr.reify)

let () = Expr.(
  let x, y = one (), zero () in
  let qs  = run q (fun z -> evalo (emax x y) z) reifys in
  let [z] = Stream.take qs in
  printf "max(1, 0) = %s\n" (show z)
)

let () = Expr.(
  let x, y = one (), zero () in
  let qs  = run q (fun z -> evalo (emax x y) z) reifys in
  let [z] = Stream.take qs in
  printf "max(1, 0) = 1 ?\n" (show z)
)

(* let () = Expr.(
  let qs = run q (fun q -> evalo (emax (one ()) (zero ())) q) (fun qs -> qs) in
  List.iter (pprint Format.std_formatter) @@ Stream.take qs
) *)
