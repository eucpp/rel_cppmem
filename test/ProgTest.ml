open MiniKanren
open MiniKanrenStd

open Lang
open Lang.Expr
open Lang.Stmt
open Lang.Loc
open Lang.Reg
open Lang.Value

let prog_test ~name ~prog ~init ~final =
  let test () =
    let inputo  = (===) (RegStorage.from_assoc init) in
    let asserto _ = (===) (RegStorage.from_assoc final) in
    let stream = Query.verify ThreadSubSys.intrpo inputo asserto prog in
    if Stream.is_empty stream then
      Test.Ok
    else
      let ff = Format.std_formatter in
      let module Trace = Utils.Trace(RegStorage) in
      Format.fprintf ff "Test %s fails!@;" name;
      let cexs = Stream.take stream in
      Format.fprintf ff "List of counterexamples:@;";
      List.iter (fun (_, cex) -> Format.fprintf ff "%a@;" Trace.trace cex) cexs;
      Test.Fail ""
  in
  Test.make_testcase ~name ~test

let test_assign = prog_test
  ~name:"assign"
  ~init: [("r1", 0)]
  ~final:[("r1", 1)]
  ~prog:<:cppmem<
    r1 := 1
  >>

let test_if_true = prog_test
  ~name:"if-true"
  ~init: [("r1", 1); ("r2", 0)]
  ~final:[("r1", 1); ("r2", 1)]
  ~prog:<:cppmem<
    if r1 then r2 := 1 else r2 := 2 fi
  >>

let test_if_false = prog_test
  ~name:"if-false"
  ~init: [("r1", 0); ("r2", 0)]
  ~final:[("r1", 0); ("r2", 2)]
  ~prog:<:cppmem<
    if r1 then r2 := 1 else r2 := 2 fi
  >>

let test_repeat = prog_test
  ~name:"repeat"
  ~init: [("r1", 0)]
  ~final:[("r1", 3)]
  ~prog:<:cppmem<
    repeat r1 := (r1 + 1) until (r1 = 3)
  >>

let tests = Test.(
  make_testsuite ~name:"Prog" ~tests: [
    test_assign;
    test_if_true;
    test_if_false;
    test_repeat;
  ]
)
