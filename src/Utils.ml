open MiniKanren

let zip3 xs ys zs = Stream.map (fun (x, (y, z)) -> (x, y, z)) @@ Stream.zip xs @@ Stream.zip ys zs

(* let excl_answ qs =
  assert ( not (Stream.is_empty qs));
  let
    (hd, tl) = Stream.retrieve ~n:1 qs
  in
    (** We should get deterministic result *)
    (* assert (Stream.is_empty tl); *)
    List.hd hd

let show_assoc show_k show_v lst =
  let content = List.fold_left (fun ac (k, v) -> ac ^ " {" ^ (show_k k) ^ ": " ^ (show_v v) ^ "}; ") "" lst in
    "{" ^ content ^ "}"

let eq_assoc eq_k eq_v assoc assoc' =
  let
    check_exists (k, v) = List.exists (fun (k', v') -> (eq_k k k') && (eq_v v v')) assoc'
  in
    List.for_all check_exists assoc

let inj_assoc inj_k inj_v assoc = MiniKanren.List.inj (fun (k, v) -> !!(inj_k k, inj_v v)) @@ MiniKanren.List.of_list assoc

let prj_pair prj_a prj_b pair =
  let (a, b) = !?pair in (prj_a a, prj_b b)

let prj_assoc prj_k prj_v assoc = MiniKanren.List.to_list @@ MiniKanren.List.prj (fun lpair -> prj_pair prj_k prj_v @@ lpair) assoc

let key_eqo k p b =
  fresh (k' v')
    (p === !!(k', v'))
    (conde [
      ((k === k') &&& (b === !!true));
      ((k =/= k') &&& (b === !!false))
    ])

let key_not_eqo k p b = conde [
    (b === !!true)  &&& (key_eqo k p !!false);
    (b === !!false) &&& (key_eqo k p !!true);
  ]

let assoco k assocs v =
  fresh (opt)
    (MiniKanren.List.lookupo (key_eqo k) assocs opt)
    (opt === !!(Some !!(k, v)))

let assoc_defaulto k assocs def v =
  fresh (opt)
    (MiniKanren.List.lookupo (key_eqo k) assocs opt)
    (conde [
         (opt === !!(Some !!(k, v)));
         (opt === !!None) &&& (v === def);
    ])


let remove_assoco k assocs assocs' =
  MiniKanren.List.filtero (key_not_eqo k) assocs assocs'

let rec update_assoco_k k upd_v assocs assocs' = conde [
  fresh (v)
    (assocs === !!MiniKanren.Nil)
    (upd_v k !!None v)
    (assocs' === !!(k, v) % !!MiniKanren.Nil);

  fresh (hd tl tl' k' v v')
    (assocs === !!(MiniKanren.Cons (hd, tl)))
    (hd === !!(k', v'))
    (conde [
      (k === k') &&& (assocs' === !!(MiniKanren.Cons (!!(k , v ), tl ))) &&& (upd_v k !!(Some v') v);
      (k =/= k') &&& (assocs' === !!(MiniKanren.Cons (!!(k', v'), tl'))) &&& (update_assoco_k k upd_v tl tl');
    ])
]

let rec update_assoco k v assocs assocs' =
  let
    upd_v k opt vnew = (vnew === v)
  in
    update_assoco_k k upd_v assocs assocs'

let maxo a b c = conde [
  fresh (f)
    (Nat.gto a b f)
    (conde [
      (f === !!true ) &&& (a === c);
      (f === !!false) &&& (b === c);
    ])
] *)

module Option =
  struct
    exception No_value

    let is_some = function
      | Some _  -> true
      | None    -> false

    let is_none = function
      | Some _ -> false
      | None   -> true

    let get = function
      | Some x -> x
      | None   -> raise No_value
  end
