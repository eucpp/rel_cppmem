open MiniKanren
open MiniKanrenStd

open Lang
open Lang.Expr
open Lang.Stmt
open Lang.Loc
open Lang.Reg
open Lang.Value

let prog_test ~name ~prog ~check =
  let test () =
    let open SequentialInterpreter in
    let module Trace = Utils.Trace(State) in
    let state = State.init @@ Regs.alloc ["r1"; "r2"; "r3"; "r4"] in
    let lst = Stream.take @@ Query.exec interpo prog state in
    let len = List.length lst in
    if len <> 1 then begin
      Format.printf "Test %s fails: number of results %d!@;" name len;
      Format.printf "List of results:@;";
      List.iter (fun res -> Format.printf "%a@;" Trace.trace res) lst;
      Test.Fail ""
      end
    else
      let inputo = (===) state in
      (* ignore input in assert, consider only output *)
      let asserto _ o =
        fresh (rs)
          (State.regso o rs)
          (Regs.checko rs check)
      in
      let stream = Query.verify ~interpo ~asserto inputo prog in
      if Stream.is_empty stream then
        Test.Ok
      else begin
        Format.printf "Test %s fails!@;" name;
        let cexs = Stream.take stream in
        Format.printf "List of counterexamples:@;";
        List.iter (fun (_, cex) -> Format.printf "%a@;" Trace.trace cex) cexs;
        Test.Fail ""
      end
  in
  Test.make_testcase ~name ~test

let test_assign = prog_test
  ~name:"assign"
  ~check:[("r1", 1)]
  ~prog:<:cppmem<
    r1 := 1
  >>

let test_seq = prog_test
  ~name:"seq"
  ~check:[("r1", 1); ("r2", 1)]
  ~prog:<:cppmem<
    r1 := 1;
    r2 := 1
  >>

let test_if_true = prog_test
  ~name:"if-true"
  ~check:[("r1", 1); ("r2", 0)]
  ~prog:<:cppmem<
    if 1 then r1 := 1 else r2 := 1 fi
  >>

let test_if_false = prog_test
  ~name:"if-false"
  ~check:[("r1", 0); ("r2", 1)]
  ~prog:<:cppmem<
    if 0 then r1 := 1 else r2 := 1 fi
  >>

let test_repeat = prog_test
  ~name:"repeat"
  ~check:[("r1", 3)]
  ~prog:<:cppmem<
    repeat r1 := (r1 + 1) until (r1 = 3)
  >>

let tests = Test.(
  make_testsuite ~name:"Prog" ~tests: [
    test_assign;
    test_seq;
    test_if_true;
    test_if_false;
    test_repeat;
  ]
)
