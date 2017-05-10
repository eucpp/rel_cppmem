open MiniKanren

let rec pprint_logic pp ff = function
  | Value x         -> Format.fprintf ff "%a" pp x
  | Var (i, [])  -> Format.fprintf ff "?%d" i
  | Var (i, ctrs)  ->
    Format.fprintf ff "?%d{" i;
    List.iter (fun ctr -> Format.fprintf ff "=/= %a; " (pprint_logic pp) ctr) ctrs;
    Format.fprintf ff "}"

let pprint_llist'' pp pp_tail ff = function
  | Cons (x, xs) -> Format.fprintf ff "%a;@;<1>%a" pp x pp_tail xs
  | Nil          -> ()

let rec pprint_llist' pp ff xs =
  pprint_logic (pprint_llist'' pp (pprint_llist' pp)) ff xs

let pprint_llist_generic fmt fmt_cell pp ff xs =
  Format.fprintf ff fmt (pprint_llist' fmt_cell pp) xs

(* let pprint_llist = pprint_llist_generic "@[<h>[ %a]@]" "%a; %a" *)
let pprint_llist pp ff xs = Format.fprintf ff "@[<hv>[ %a]@]" (pprint_llist' pp) xs

let rec pprint_nat ff n =
  let pp ff _ = Format.fprintf ff "%d" (Nat.to_int @@ Nat.from_logic n) in
  pprint_logic pp ff n

let pprint_string = pprint_logic (fun ff s -> Format.fprintf ff "%s" s)

let zip3 xs ys zs = Stream.map (fun (x, (y, z)) -> (x, y, z)) @@ Stream.zip xs @@ Stream.zip ys zs

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
