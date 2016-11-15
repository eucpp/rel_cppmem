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
