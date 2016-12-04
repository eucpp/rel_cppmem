open OUnit2
open MiniKanren
open Memory
open Rules

module ET = Lang.ExprTerm
module EC = Lang.ExprContext

module Tester 
  (L : Lang.Lang with type lt = ET.lt with type lc = EC.lc with type ls = Registers.lt) = 
  struct 
    module Sem = Semantics.Make(L)

    let step sem t s = 
      run qr (fun q  r  -> Sem.stepo sem (L.inj_term t) (L.inj_st s) q r)
             (fun qs rs -> Stream.zip (Stream.map L.prj_term qs) (Stream.map L.prj_st rs))

    let test_rule rule (t, s) expected test_ctx =
      let sem = Sem.make [rule] in
      let stream = step sem t s in
      let (actual, stream') = Stream.retrieve ~n:(List.length expected) @@ stream in
      let show (t, s)        = "Term/State is not found among answers: " ^ L.show_term t ^ "; " ^ L.show_st s in
      let eq (t, s) (t', s') = (L.eq_term t t') && (L.eq_st s s') in
      let assert_contains x = 
        assert_bool (show x) @@ List.exists (eq x) actual
      in
        assert_bool "More answers than expected" (Stream.is_empty stream');
        List.iter assert_contains expected
  end
  
module BasicExprTester = Tester(Lang.ExprLang)

let regs = Registers.set "x" 42 Registers.empty

let basic_expr_tests = 
  "basic_expr">::: [
    "var">:: BasicExprTester.test_rule BasicExpr.varo (ET.Var "x", regs) [(ET.Const 42, regs)]
  ]

let tests = 
  "rules">::: [basic_expr_tests]
