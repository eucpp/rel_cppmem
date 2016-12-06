let excl_answ qs = 
  assert ( not (MiniKanren.Stream.is_empty qs)); 
  let 
    (hd, tl) = MiniKanren.Stream.retrieve ~n:1 qs
  in
    (** We should get deterministic result *)
    assert (MiniKanren.Stream.is_empty tl);
    List.hd hd

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
