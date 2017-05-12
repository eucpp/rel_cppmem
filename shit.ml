open MiniKanren
open Lang
open Lang.Term

let t = fun q ->
<:cppmem<
  spw {{{
      ? q := 2;
      x_rel := 1 + 2;
      r := y_acq
    |||
      if x then 1 else 0 fi;
      repeat 1 end
    }}}
>>

 (* let t = skip *)

let _ =
  let some_fuck = pprint @@ to_logic @@ prj (t @@ var !!"r") in
  print_endline some_fuck

(* let _ =
  try
    failwith "this is not an error"
  finally
    print_endline "OK" *)

(* let _ = fuck success *)
