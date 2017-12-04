#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Pcaml
open Printf
(* open MiniKanren *)
open Lang
open Lang.Term
open Lang.Expr

(* let op s = <:expr<  >> *)

let gram = Grammar.gcreate (Plexer.gmake ());;

let cppmem_eoi      = Grammar.Entry.create gram "cppmem";;
let cppmem_expr     = Grammar.Entry.create gram "cppmem";;
let cppmem_stmt     = Grammar.Entry.create gram "cppmem";;
let cppmem_antiquot = Grammar.Entry.create gram "cppmem";;

EXTEND
  cppmem_eoi: [ [ x = cppmem_stmt; EOI -> x ] ];

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
        <:expr< var (Register.reg $str:x$) >>

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
      [ t1 = cppmem_stmt; ";"; t2 = cppmem_stmt ->
        <:expr< seq $t1$ $t2$ >>

      | "spw"; "{";"{";"{"; t1 = cppmem_stmt; "|||"; t2 = cppmem_stmt; "}";"}";"}" ->
        <:expr< spw $t1$ $t2$ >>

      | "par"; "{";"{";"{"; t1 = cppmem_stmt; "|||"; t2 = cppmem_stmt; "}";"}";"}" ->
        <:expr< par $t1$ $t2$ >>

      ]

    | [ "assert"; "("; e = cppmem_expr; ")" ->
        <:expr< assertion $e$ >>

      | "skip" ->
        <:expr< skip () >>

      | x = LIDENT; ":="; e = cppmem_expr ->
        if String.contains x '_' then
          let l::mo::[] = String.split_on_char '_' x in
          <:expr< store (MemOrder.mo $str:mo$) (Loc.loc $str:l$) $e$ >>
        else
        <:expr< asgn (Register.reg $str:x$) $e$ >>

        (* | "load"; x = LIDENT; r = LIDENT ->
          let l::mo::[] = String.split_on_char '_' x in
          <:expr< load (MemOrder.mo $str:mo$) (Loc.loc $str:l$) (Register.reg $str:r$) >>

        | "store"; x = LIDENT; e = cppmem_expr ->
          let l::mo::[] = String.split_on_char '_' x in
          <:expr< store (MemOrder.mo $str:mo$) (Loc.loc $str:l$) $e$ >> *)

      | x = LIDENT; ":="; y = LIDENT ->
        if String.contains y '_' then
          let l::mo::[] = String.split_on_char '_' y in
          <:expr< load (MemOrder.mo $str:mo$) (Loc.loc $str:l$) (Register.reg $str:x$) >>
        else
          if String.contains x '_' then
            let l::mo::[] = String.split_on_char '_' x in
            <:expr< store (MemOrder.mo $str:mo$) (Loc.loc $str:l$) (var (Register.reg $str:y$)) >>
          else
            assert false

      | "if"; e = cppmem_expr; "then"; t1 = cppmem_stmt; "else"; t2 = cppmem_stmt; "fi" ->
        <:expr< if' $e$ $t1$ $t2$ >>

      | "repeat"; t = cppmem_stmt; "until"; e = cppmem_expr ->
        <:expr< repeat $t$ $e$ >>

      | "?"; q = cppmem_antiquot -> q

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
