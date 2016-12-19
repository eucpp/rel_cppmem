open OUnit2
open MiniKanren
open Memory
open Rules

module ET = Lang.ExprTerm
module EC = Lang.ExprContext
module ES = Lang.ExprState

module ST = Lang.StmtTerm
module SC = Lang.StmtContext
module SS = Lang.StmtState

module Tester 
  (T : Lang.Term)
  (C : Lang.Context with type t = T.t with type lt' = T.lt')
  (S : Lang.State) = 
  struct 
    module Sem = Semantics.Make(T)(C)(S)

    let show (t, s) = "Term/State is not found among answers: " ^ T.show t ^ "; " ^ S.show s
      
    let eq (t, s) (t', s') = (T.eq t t') && (S.eq s s')

    let test_step rules (t, s) expected test_ctx =
      let sem    = Sem.make rules in
      let stream = Sem.step sem t s in
        TestUtils.assert_stream stream expected ~show:show ~eq:eq

    let test_space rules (t, s) expected test_ctx = 
      let sem    = Sem.make rules in
      let stream = Sem.space sem t s in
        TestUtils.assert_stream stream expected ~show:show ~eq:eq 
  end
  
module BasicExprTester = Tester(ET)(EC)(ES)
module BasicStmtTester = Tester(ST)(SC)(SS)

let regs = Registers.set "x" 42 Registers.empty

let thrd = { ThreadState.regs = regs; ThreadState.curr = ViewFront.empty }

let basic_expr_tests = 
  "basic_expr">::: [
    "var">:: BasicExprTester.test_step [BasicExpr.var] (ET.Var "x", thrd) [(ET.Const 42, thrd)];
    
    "binop">:: BasicExprTester.test_step [BasicExpr.binop] (ET.Binop ("+", ET.Const 1, ET.Const 2), thrd) [(ET.Const 3, thrd)];

    "step_all">:: BasicExprTester.test_step BasicExpr.all (ET.Binop ("+", ET.Var "x", ET.Const 1), thrd) [(ET.Binop ("+", ET.Const 42, ET.Const 1), thrd)];


    "space_all">:: let
               e = (ET.Binop ("+", ET.Var "x", ET.Binop ("*", ET.Const 2, ET.Const 4)), thrd)
             in
               BasicExprTester.test_space BasicExpr.all e [(ET.Const 50, thrd); (ET.Const 50, thrd)];
  ]

let mem = MemState.assign_local Path.N "x" 42 MemState.empty

let basic_stmt_tests = 
  "basic_stmt">::: [
    "expr"  >:: BasicStmtTester.test_step [BasicStmt.expr] (ST.AExpr (ET.Var "x"), mem) [(ST.AExpr (ET.Const 42), mem)];

    "assign">:: BasicStmtTester.test_step [BasicStmt.asgn] (ST.Asgn (ST.AExpr (ET.Var "x"), ST.AExpr (ET.Const 42)), MemState.empty) [(ST.Skip, mem)];

    "if_true">:: BasicStmtTester.test_step [BasicStmt.if'] (ST.If (ET.Var "x", ST.Skip, ST.Stuck), mem) [(ST.Skip, mem)];

    "if_false">:: BasicStmtTester.test_step [BasicStmt.if'] (ST.If (ET.Const 0, ST.Stuck, ST.Skip), mem) [(ST.Skip, mem)];

    "while">:: (let loop = ST.While (ET.Const 1, ST.Skip) in
                  BasicStmtTester.test_step [BasicStmt.while'] (loop, mem) [(ST.If (ET.Const 1, ST.Seq (ST.Skip, loop), ST.Skip), mem)]);

    "seq_skip">:: BasicStmtTester.test_step [BasicStmt.seq] (ST.Seq (ST.Skip, ST.Skip), mem) [(ST.Skip, mem)];

    "seq_stuck">:: BasicStmtTester.test_step [BasicStmt.seq] (ST.Seq (ST.Stuck, ST.Skip), mem) [(ST.Stuck, mem)];

    "spawn">:: (let leaf = ThreadTree.Leaf ThreadState.empty in
                let thrd_tree = ThreadTree.Node (leaf, leaf) in
                let state  = { MemState.thrds = leaf; MemState.story = MemStory.empty; } in
                let state' = { MemState.thrds = thrd_tree; MemState.story = MemStory.empty } in
                  BasicStmtTester.test_step [BasicStmt.spawn] (ST.Spw (ST.Skip, ST.Skip), state) [(ST.Par (ST.Skip, ST.Skip), state')]);

    "join">::  (let leaf = ThreadTree.Leaf ThreadState.empty in
                let thrd_tree = ThreadTree.Node (leaf, leaf) in
                let state  = { MemState.thrds = thrd_tree;  MemState.story = MemStory.empty; } in
                let state' = { MemState.thrds = leaf;  MemState.story = MemStory.empty; } in
                  BasicStmtTester.test_step [BasicStmt.join] (ST.Par (ST.AExpr (ET.Const 1), ST.AExpr (ET.Const 2)), state) [(ST.Pair (ET.Const 1, ET.Const 2), state')]);
  ]

let tests = 
  "rules">::: [basic_expr_tests; basic_stmt_tests]
