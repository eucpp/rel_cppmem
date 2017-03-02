open OUnit2
open MiniKanren
open Memory

module T = Lang.Term
module Sem = Semantics.Make(Lang.Term)(Lang.Context)(MemState)

let sem = Sem.make @@ List.append Rules.Basic.all Rules.RelAcq.all

let show s = "Outcome is not found among answers: " ^ s 
      
let eq s s = s == s

let test_prog prog expected test_ctx =
  let lexbuf  = Lexing.from_string prog in
  let term    = Parser.main Lexer.token lexbuf in
  (* let _ = assert (term = T.Seq (T.Write (REL, "x", T.Const 0), T.Write (REL, "y", T.Const 0))) in  *)
  let stream  = Sem.space sem term MemState.empty in
  let stream' = Stream.map (fun (t, s) -> T.show t) stream in
    (* print_endline @@ T.show term; *)
    (* Stream.iter (fun s -> print_endline s) stream'; *)
    TestUtils.assert_stream ~empty_check:false stream' expected ~show:show ~eq:eq

let prog_LB = "
    x_rel := 0;
    y_rel := 0;
    spw {{{
        r1 := x_acq;
        y_rel := 1;
        ret r1
    |||
        r2 := y_acq;
        x_rel := 1;
        ret r2
    }}}"

let test_LB =
  "LB">: OUnitTest.TestCase (OUnitTest.Long, test_prog prog_LB ["(0, 0)"; "(1, 0)"; "(0, 1)"])
  (* "simple">:: test_prog "x_rel := 0 ; y_rel := 0" ["skip"] *)

let tests = 
  "relAcq">::: [test_LB]
