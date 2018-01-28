#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Pcaml
open Printf
(* open MiniKanren *)
open Relcppmem.Lang
(* open Lang.Term *)
(* open Lang.Expr *)

(* let op s = <:expr<  >> *)

let gram = Grammar.gcreate (Plexer.gmake ());;

let cppmem_eoi      = Grammar.Entry.create gram "cppmem";;
let cppmem_expr     = Grammar.Entry.create gram "cppmem";;
let cppmem_stmt     = Grammar.Entry.create gram "cppmem";;
let cppmem_prog     = Grammar.Entry.create gram "cppmem";;
let cppmem_cprog    = Grammar.Entry.create gram "cppmem";;
let cppmem_antiquot = Grammar.Entry.create gram "cppmem";;

EXTEND
  cppmem_eoi: [ [ x = cppmem_prog; EOI -> x ] ];

  cppmem_expr:
    [ [ x = cppmem_expr; "&&"; y = cppmem_expr ->
        <:expr< binop (Bop.bop "&&") $x$ $y$ >>

      | x = cppmem_expr; "||"; y = cppmem_expr ->
        <:expr< binop (Bop.bop "||") $x$ $y$ >>
      ]

    | [ x = cppmem_expr; "="; y = cppmem_expr ->
        <:expr< binop (Bop.bop "=") $x$ $y$ >>

      | x = cppmem_expr; "!="; y = cppmem_expr ->
        <:expr< binop (Bop.bop "!=") $x$ $y$ >>

      | x = cppmem_expr; "<"; y = cppmem_expr ->
        <:expr< binop (Bop.bop "<") $x$ $y$ >>

      | x = cppmem_expr; ">"; y = cppmem_expr ->
        <:expr< binop (Bop.bop ">") $x$ $y$ >>

      | x = cppmem_expr; "<="; y = cppmem_expr ->
        <:expr< binop (Bop.bop "<=") $x$ $y$ >>

      | x = cppmem_expr; ">="; y = cppmem_expr ->
        <:expr< binop (Bop.bop ">=") $x$ $y$ >>
      ]

    | [ x = cppmem_expr; "+"; y = cppmem_expr ->
        <:expr< binop (Bop.bop "+") $x$ $y$ >>
      ]

    | [ x = cppmem_expr; "*"; y = cppmem_expr ->
        <:expr< binop (Bop.bop "*") $x$ $y$ >>
      ]

    | [ n = INT ->
        <:expr< const (Value.integer $int:n$) >>

      | x = LIDENT ->
        <:expr< var (Reg.reg $str:x$) >>

      | "!"; e = cppmem_expr ->
        <:expr< unop (Uop.uop "!") $e$ >>

      (* | "CAS"; "("; mo1 = LIDENT; ","; mo2 = LIDENT; ","; x = LIDENT; ","; expected = INT; ","; desired = INT; ")" ->
        let mo1 = <:expr< MemOrder.mo $str:mo1$ >> in
        let mo2 = <:expr< MemOrder.mo $str:mo2$ >> in
        let expected = <:expr< const (Value.integer $int:expected$) >> in
        let desired = <:expr< const (Value.integer $int:desired$) >> in
        <:expr< cas $mo1$ $mo2$ (Loc.loc $str:x$) $expected$ $desired$ >> *)

      | "("; x = cppmem_expr; ")" -> x
      ]
    ];

  cppmem_stmt:
    [ RIGHTA
      [ "assert"; "("; e = cppmem_expr; ")" ->
        <:expr< assertion $e$ >>

      | x = LIDENT; ":="; e = cppmem_expr ->
        if String.contains x '_' then
          let l::mo::[] = String.split_on_char '_' x in
          <:expr< store (MemOrder.mo $str:mo$) (Loc.loc $str:l$) $e$ >>
        else
        <:expr< asgn (Reg.reg $str:x$) $e$ >>

        (* | "load"; x = LIDENT; r = LIDENT ->
          let l::mo::[] = String.split_on_char '_' x in
          <:expr< load (MemOrder.mo $str:mo$) (Loc.loc $str:l$) (Reg.reg $str:r$) >>

        | "store"; x = LIDENT; e = cppmem_expr ->
          let l::mo::[] = String.split_on_char '_' x in
          <:expr< store (MemOrder.mo $str:mo$) (Loc.loc $str:l$) $e$ >> *)

      | x = LIDENT; ":="; y = LIDENT ->
        if String.contains y '_' then
          let l::mo::[] = String.split_on_char '_' y in
          <:expr< load (MemOrder.mo $str:mo$) (Loc.loc $str:l$) (Reg.reg $str:x$) >>
        else
          if String.contains x '_' then
            let l::mo::[] = String.split_on_char '_' x in
            <:expr< store (MemOrder.mo $str:mo$) (Loc.loc $str:l$) (var (Reg.reg $str:y$)) >>
          else
            assert false

      | "if"; e = cppmem_expr; "then"; t1 = cppmem_prog; "else"; t2 = cppmem_prog; "fi" ->
        <:expr< if' $e$ $t1$ $t2$ >>

      | "repeat"; t = cppmem_prog; "until"; e = cppmem_expr ->
        <:expr< repeat $t$ $e$ >>

      | "return"; "("; regs = LIST0 LIDENT SEP ","; ")" ->
        let string_list loc lst =
          List.fold_right
          (fun head tail -> <:expr< [ $str:head$ :: $tail$ ] >>)
          lst
          <:expr< [] >>
        in
        let regs = string_list loc regs in
        let regs =
          <:expr< MiniKanren.Std.List.list (List.map (fun r -> Reg.reg r) $regs$) >>
        in
        <:expr< return ($regs$) >>

      | "?"; q = cppmem_antiquot -> q

      ]

    ];

  cppmem_prog:
    [ [ stmts = LIST0 cppmem_stmt SEP ";" ->
        let stmt_list loc lst =
          List.fold_right
          (fun head tail -> <:expr< [ $head$ :: $tail$ ] >>)
          lst
          <:expr< [] >>
        in
        let stmts = stmt_list loc stmts in
        <:expr< prog ($stmts$) >>
      ]
    ];

    (* [ "spw"; "{";"{";"{"; t1 = cppmem_prog; "|||"; t2 = cppmem_prog; "}";"}";"}" ->
      <:expr< spw $t1$ $t2$ >>
    ]

  |  *)

  cppmem_cprog:
    [ [ "spw"; "{";"{";"{"; progs = LIST1 cppmem_prog SEP "|||"; "}";"}";"}"; EOI ->
        let progs_list loc lst =
          List.fold_right
          (fun head tail -> <:expr< [ $head$ :: $tail$ ] >>)
          lst
          <:expr< [] >>
        in
        let progs = progs_list loc progs in
        <:expr< cprog ($progs$) >>
      ]
    ];

  cppmem_antiquot:
    [ [ x = LIDENT ->
        let ast =
          let loc = Ploc.make_unlined (0, String.length x) in
          <:expr< $lid:x$ >>
        in
        <:expr< $anti:ast$ >>
      ]
    ];

END;;

let cppmem_exp s = Grammar.Entry.parse cppmem_eoi (Stream.of_string s);;
let cppmem_pat s = failwith "not implemented cppmem_pat";;
Quotation.add "cppmem" (Quotation.ExAst (cppmem_exp, cppmem_pat));;

let cppmem_par_exp s = Grammar.Entry.parse cppmem_cprog (Stream.of_string s);;
let cppmem_par_pat s = failwith "not implemented cppmem_par_pat";;
Quotation.add "cppmem_par" (Quotation.ExAst (cppmem_par_exp, cppmem_par_pat));;
