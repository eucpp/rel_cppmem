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

    let step sem t s = 
      run qr (fun q  r  -> Sem.stepo sem (T.inj t) (S.inj s) q r)
             (fun qs rs -> Stream.zip (Stream.map T.prj qs) (Stream.map S.prj rs))

    let test_rule rule (t, s) expected test_ctx =
      let sem = Sem.make [rule] in
      let stream = step sem t s in
      let (actual, stream') = Stream.retrieve ~n:(List.length expected) @@ stream in
      let show (t, s)        = "Term/State is not found among answers: " ^ T.show t ^ "; " ^ S.show s in
      let eq (t, s) (t', s') = (T.eq t t') && (S.eq s s') in
      let assert_contains x = 
        assert_bool (show x) @@ List.exists (eq x) actual
      in
        assert_bool "More answers than expected" (Stream.is_empty stream');
        List.iter assert_contains expected
  end
  
module BasicExprTester = Tester(ET)(EC)(ES)

let regs = Registers.set "x" 42 Registers.empty

let basic_expr_tests = 
  "basic_expr">::: [
    "var">:: BasicExprTester.test_rule BasicExpr.var (ET.Var "x", regs) [(ET.Const 42, regs)];
    
    "binop">:: BasicExprTester.test_rule BasicExpr.binop (ET.Binop ("+", ET.Const 1, ET.Const 2), regs) [(ET.Const 3, regs)]
  ]

let tests = 
  "rules">::: [basic_expr_tests]
