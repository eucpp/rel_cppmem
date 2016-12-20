open OUnit2
open MiniKanren

let assert_contains xs x 
                    ?(show = fun _ -> "Argument is not found among answers") 
                    ?(eq = (=)) = 
  assert_bool (show x) @@ List.exists (eq x) xs

let assert_stream ?(empty_check = true) stream expected ?show ?eq =
  let len               = List.length expected in
  let (actual, stream') = Stream.retrieve ~n:len stream in
    (if empty_check 
     then 
        assert_bool "More answers than expected" @@ Stream.is_empty stream');
    assert_bool "Less answers than expected" (len = (List.length actual));
    List.iter (assert_contains actual ?show ?eq) expected 
   
