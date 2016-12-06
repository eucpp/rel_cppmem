open OUnit2
open MiniKanren
open Memory
open Rules

module ET = Lang.ExprTerm
module EC = Lang.ExprContext
module ES = Lang.ExprState

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

let regs = Registers.set "x" 42 Registers.empty

let basic_expr_tests = 
  "basic_expr">::: [
    "var">:: BasicExprTester.test_step [BasicExpr.var] (ET.Var "x", regs) [(ET.Const 42, regs)];
    
    "binop">:: BasicExprTester.test_step [BasicExpr.binop] (ET.Binop ("+", ET.Const 1, ET.Const 2), regs) [(ET.Const 3, regs)];

    "step_all">:: BasicExprTester.test_step BasicExpr.all (ET.Binop ("+", ET.Var "x", ET.Const 1), regs) [(ET.Binop ("+", ET.Const 42, ET.Const 1), regs)];


    "space_all">:: let
               e = (ET.Binop ("+", ET.Var "x", ET.Binop ("*", ET.Const 2, ET.Const 4)), regs)
             in
               BasicExprTester.test_space BasicExpr.all e [(ET.Const 50, regs); (ET.Const 50, regs)]
  ]

let tests = 
  "rules">::: [basic_expr_tests]
