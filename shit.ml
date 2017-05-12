open MiniKanren
open Lang.Term

let _ =
  let some_fuck = pprint @@ to_logic @@ prj (skip_me) in
  print_endline some_fuck

(* let _ =
  try
    failwith "this is not an error"
  finally
    print_endline "OK" *)

(* let _ = fuck success *)
