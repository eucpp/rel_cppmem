open MiniKanren
open MiniKanrenStd

type ('a, 'b) tt = ('a * 'b) List.ground
type ('a, 'b) tl = ('a * 'b) logic List.logic

type ('a, 'b, 'c, 'd) ti = (('a * 'b), ('c * 'd) logic) MiniKanrenStd.List.groundi

let inj inj_a inj_b = inj_list (fun (a, b) -> pair (inj_a a) (inj_b b))

let allocate vars default = inj_listi @@ List.map (fun var -> pair var default) vars

let key_eqo k p b =
  fresh (k' v')
    (p === pair k' v')
    (conde [
      ((k === k') &&& (b === !!true));
      ((k =/= k') &&& (b === !!false))
    ])

let rec geto vars var value =
  fresh (hd tl)
    (vars === hd % tl)
    (conde [
      (hd === pair var value);
      (hd =/= pair var value) &&& (geto tl var value);
    ])

let rec seto vars vars' var value =
  fresh (hd tl tl' k v)
    (vars === hd % tl)
    (hd === pair k v)
    (conde [
      (k === var) &&& (vars' === (pair var value) % tl);
      (k =/= var) &&& (vars' === hd % tl') &&& (seto tl tl' var value);
    ])

let join_tso p1 p2 p' = Nat.(
  fresh (var ts1 ts2 ts')
    (p1 === pair var ts1)
    (p2 === pair var ts2)
    (conde [
        (ts1 >  ts2) &&& (p' === p1);
        (ts1 <= ts2) &&& (p' === p2);
    ])
  )

let rec mapo relo vars vars' = conde [
  (vars === nil ()) &&& (vars' === nil ());
  (fresh (hd tl hd' vars'')
    (vars === hd % tl)
    (relo hd hd')
    (vars' === hd' % vars'')
    (mapo relo tl vars''));
  ]

let rec map2o relo vars1 vars2 vars' = conde [
  (vars1 === nil ()) &&& (vars2 === nil ()) &&& (vars' === nil ());
  (fresh (hd1 tl1 hd2 tl2 hd' vars'')
    (vars1 === hd1 % tl1)
    (vars2 === hd2 % tl2)
    (relo hd1 hd2 hd')
    (vars' === hd' % vars'')
    (map2o relo tl1 tl2 vars''));
  ]
