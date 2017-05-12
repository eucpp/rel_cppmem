open MiniKanren
open Lang
open Lang.Term

let t = cppmem {
  if 1 = 1 then 1 else 0 fi
}

 (* let t = skip *)

let _ =
  let some_fuck = pprint @@ to_logic @@ prj t in
  print_endline some_fuck

(* let _ =
  try
    failwith "this is not an error"
  finally
    print_endline "OK" *)

(* let _ = fuck success *)
