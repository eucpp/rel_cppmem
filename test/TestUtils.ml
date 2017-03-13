open OUnit2
open MiniKanren
open Memory

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

module Sem = Semantics.Make(Lang.Term)(Lang.Context)(MemState)

let test_prog sem prog expected test_ctx =
  let show s = "Outcome is not found among answers: " ^ s in
  let lexbuf  = Lexing.from_string prog in
  let term    = Parser.main Lexer.token lexbuf in
  let state   = {MemState.thrds = ThreadTree.empty;
                 MemState.story = MemStory.empty;
                 MemState.scmem = SCMemory.preallocate term; } in
  let stream =
   run qrs (fun q  r  s  ->
              (* fresh (s') *)
                (Sem.spaceo sem (Lang.Term.inj term) (MemState.inj state) q r nil))
                (* (List.appendo s' nil s)) *)
           (fun qs rs ss -> Utils.zip3
             (Stream.map Lang.Term.prj qs)
             (Stream.map MemState.prj rs)
             ss) in
  let stream' = Stream.map (fun (t, s, epath) -> Lang.Term.show t) stream in
  let cnt     = ref 0 in
    Stream.iter (
        fun (t, s, epath) -> cnt := !cnt + 1;
        Printf.printf "\n%d" !cnt
        (* Printf.printf "\n%d: %s\n" !cnt (String.concat " -> " epath) *)
      ) stream;
    assert_stream ~empty_check:false stream' expected ~show:show ~eq:(=)
