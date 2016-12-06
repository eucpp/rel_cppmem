open OUnit2
open MiniKanren

let assert_contains xs x ?show ?eq = 
  let show' = match show with 
              | None -> fun _ -> "Argument is not found among answers"
              | Some f -> f
  in
  let eq' = match eq with 
              | None -> (=)
              | Some f -> f
  in
    assert_bool (show' x) @@ List.exists (eq' x) xs

let assert_stream stream expected ?show ?eq =
  let len               = List.length expected in
  let (actual, stream') = Stream.retrieve ~n:len stream in
    assert_bool "More answers than expected" (Stream.is_empty stream');
    assert_bool "Less answers than expected" (len = (List.length actual));
    List.iter (assert_contains actual ?show ?eq) expected 
   
