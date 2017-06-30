#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Pcaml
open Printf
(* open MiniKanren *)
open Lang
open Lang.Term

(* let op s = <:expr<  >> *)

let gram = Grammar.gcreate (Plexer.gmake ());;
let term_eoi = Grammar.Entry.create gram "cppmem";;
let term = Grammar.Entry.create gram "cppmem";;
let term_antiquot = Grammar.Entry.create gram "cppmem";;
EXTEND
  term_eoi: [ [ x = term; EOI -> x ] ];
  term: [ [
        n = INT   ->
        <:expr< const ( Value.inj (Value.of_string $str:n$)) >>

      | x = LIDENT ->
        if String.contains x '_' then
          let var::mo::[] = String.split_on_char '_' x in
          <:expr< read (MemOrder.inj (MemOrder.of_string $str:mo$)) (Var.inj (Var.of_string $str:var$)) >>
        else
          <:expr< var (Var.inj (Var.of_string $str:x$)) >>

      | x = LIDENT; ":="; t = term ->
        if String.contains x '_' then
          let var::mo::[] = String.split_on_char '_' x in
          <:expr< write (MemOrder.inj (MemOrder.of_string $str:mo$)) (Var.inj (Var.of_string $str:var$)) $t$ >>
        else
          <:expr< asgn (var (Var.inj (Var.of_string $str:x$))) $t$ >>

      | t1 = term; ":="; t2 = term ->
        <:expr< asgn $t1$ $t2$ >>

      | t1 = term; "+" ; t2 = term ->
        <:expr< binop (Var.inj (Var.of_string "+")) $t1$ $t2$ >>
      | t1 = term; "*" ; t2 = term ->
        <:expr< binop (Var.inj (Var.of_string "*")) $t1$ $t2$ >>
      | t1 = term; "=" ; t2 = term ->
        <:expr< binop (Var.inj (Var.of_string "=")) $t1$ $t2$ >>
      | t1 = term; "!=" ; t2 = term ->
        <:expr< binop (Var.inj (Var.of_string "!=")) $t1$ $t2$ >>
      | t1 = term; "<" ; t2 = term ->
        <:expr< binop (Var.inj (Var.of_string "<")) $t1$ $t2$ >>
      | t1 = term; "<=" ; t2 = term ->
        <:expr< binop (Var.inj (Var.of_string "<=")) $t1$ $t2$ >>
      | t1 = term; ">" ; t2 = term ->
        <:expr< binop (Var.inj (Var.of_string ">")) $t1$ $t2$ >>
      | t1 = term; ">=" ; t2 = term ->
        <:expr< binop (Var.inj (Var.of_string ">=")) $t1$ $t2$ >>

      | "("; t1 = term; ","; t2 = term; ")" ->
        <:expr< pair $t1$ $t2$ >>

      | "if"; t1 = term; "then"; t2 = term; "else"; t3 = term; "fi" ->
        <:expr< if' $t1$ $t2$ $t3$ >>

      | "repeat"; t = term; "end" ->
        <:expr< repeat $t$ >>

      | t1 = term; ";"; t2 = term ->
        <:expr< seq $t1$ $t2$ >>

      | "spw"; "{";"{";"{"; t1 = term; "|||"; t2 = term; "}";"}";"}" ->
        <:expr< spw $t1$ $t2$ >>

      | "par"; "{";"{";"{"; t1 = term; "|||"; t2 = term; "}";"}";"}" ->
        <:expr< par $t1$ $t2$ >>

      | "skip" ->
        <:expr< skip () >>

      | "ret"; t = term -> t


      | "?"; q = term_antiquot -> q
  ] ];

  term_antiquot: [ [
    x = LIDENT ->
      let ast =
        let loc = Ploc.make_unlined (0, String.length x) in
        <:expr< $lid:x$ >>
      in
      <:expr< $anti:ast$ >>
  ] ];

END;;

let cppmem_exp s = Grammar.Entry.parse term_eoi (Stream.of_string s);;
let cppmem_pat s = failwith "not implemented cppmem_pat";;
Quotation.add "cppmem" (Quotation.ExAst (cppmem_exp, cppmem_pat));;
(* Quotation.default := "term";; *)
