open OUnit2
open MiniKanren

let assert_contains show eq xs x = 
  assert_bool (show x) @@ List.exists (eq x) xs

let assert_stream ?(empty_check = true) stream expected 
                  ?(show = fun _ -> "Argument is not found among answers")
                  ?(eq = (=)) 
  =
  let len               = List.length expected in
  let (actual, stream') = Stream.retrieve ~n:len stream in
    (* List.iter (fun x -> print_endline @@ show x) actual; *)
    (if empty_check 
     then 
        assert_bool "More answers than expected" @@ Stream.is_empty stream');
    assert_bool "Less answers than expected" (len = (List.length actual));
    List.iter (assert_contains show eq actual) expected 
   
module Sem = Semantics.Make(Lang.Term)(Lang.Context)(Memory.MemState)

let test_prog sem prog expected test_ctx = 
  let show s = "Outcome is not found among answers: " ^ s in
  let lexbuf  = Lexing.from_string prog in
  let term    = Parser.main Lexer.token lexbuf in
  let stream  = Sem.space sem term Memory.MemState.empty in
  let stream' = Stream.map (fun (t, s) -> Lang.Term.show t) stream in
  let cnt     = ref 0 in 
    Stream.iter (fun s -> cnt := !cnt + 1; print_endline @@ string_of_int !cnt) stream';
    assert_stream ~empty_check:false stream' expected ~show:show ~eq:(=)
