#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Pcaml
open Printf
(* open MiniKanren *)
open Lang
open Lang.Term

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
        <:expr< binop (Op.op "&&") $x$ $y$ >>

      | x = cppmem_expr; "||"; y = cppmem_expr ->
        <:expr< binop (Op.op "||") $x$ $y$ >>
      ]

    | [ x = cppmem_expr; "="; y = cppmem_expr ->
        <:expr< binop (Op.op "=") $x$ $y$ >>

      | x = cppmem_expr; "!="; y = cppmem_expr ->
        <:expr< binop (Op.op "!=") $x$ $y$ >>

      | x = cppmem_expr; "<"; y = cppmem_expr ->
        <:expr< binop (Op.op "<") $x$ $y$ >>

      | x = cppmem_expr; ">"; y = cppmem_expr ->
        <:expr< binop (Op.op ">") $x$ $y$ >>

      | x = cppmem_expr; "<="; y = cppmem_expr ->
        <:expr< binop (Op.op "<=") $x$ $y$ >>

      | x = cppmem_expr; ">="; y = cppmem_expr ->
        <:expr< binop (Op.op ">=") $x$ $y$ >>
      ]

    | [ x = cppmem_expr; "+"; y = cppmem_expr ->
        <:expr< binop (Op.op "+") $x$ $y$ >>
      ]

    | [ x = cppmem_expr; "*"; y = cppmem_expr ->
        <:expr< binop (Op.op "*") $x$ $y$ >>
      ]

    | [ n = INT ->
        <:expr< const (Value.integer $int:n$) >>

      | x = LIDENT ->
        if String.contains x '_' then
          let var::mo::[] = String.split_on_char '_' x in
          <:expr< read (MemOrder.mo $str:mo$) (Loc.loc $str:var$) >>
        else
          <:expr< var (Register.reg $str:x$) >>

      | "CAS"; "("; mo1 = LIDENT; ","; mo2 = LIDENT; ","; x = LIDENT; ","; expected = INT; ","; desired = INT; ")" ->
        let mo1 = <:expr< MemOrder.mo $str:mo1$ >> in
        let mo2 = <:expr< MemOrder.mo $str:mo2$ >> in
        let expected = <:expr< const (Value.integer $int:expected$) >> in
        let desired = <:expr< const (Value.integer $int:desired$) >> in
        <:expr< cas $mo1$ $mo2$ (Loc.loc $str:x$) $expected$ $desired$ >>

      | "("; x = cppmem_expr; ")"
        -> x
      ]
    ];

  cppmem_stmt:
    [ [ x = LIDENT; ":="; e = cppmem_expr ->
        if String.contains x '_' then
          let var::mo::[] = String.split_on_char '_' x in
          <:expr< write (MemOrder.mo $str:mo$) (Loc.loc $str:var$) $e$ >>
        else
          <:expr< asgn (var (Register.reg $str:x$)) $e$ >>

      | "if"; e = cppmem_expr; "then"; t1 = cppmem_stmt; "else"; t2 = cppmem_stmt; "fi" ->
        <:expr< if' $e$ $t1$ $t2$ >>

      | "repeat"; e = cppmem_expr; "end" ->
        <:expr< repeat $e$ >>

      | t1 = cppmem_stmt; ";"; t2 = cppmem_stmt ->
        <:expr< seq $t1$ $t2$ >>

      | "spw"; "{";"{";"{"; t1 = cppmem_stmt; "|||"; t2 = cppmem_stmt; "}";"}";"}" ->
        <:expr< spw $t1$ $t2$ >>

      | "par"; "{";"{";"{"; t1 = cppmem_stmt; "|||"; t2 = cppmem_stmt; "}";"}";"}" ->
        <:expr< par $t1$ $t2$ >>

      | "assert"; "("; e = cppmem_expr; ")" ->
        <:expr< assertion $e$ >>

      | "skip" ->
        <:expr< skip () >>

      | "ret"; e = cppmem_expr -> e

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
