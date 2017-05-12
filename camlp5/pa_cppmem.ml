#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Pcaml
open Printf
open MiniKanren
open Lang
open Lang.Term

(* | <:expr< $anti:e$ >> ->             <:expr< $anti:s e$ >> *)
(* | <:expr< $e1$ \:= $e2$ >> -> <:expr< asgn $s e1$ $s e2$ >> *)

let rec subst_cppmem expr =
  let s = subst_cppmem in
  let loc = MLast.loc_of_expr expr in
  match expr with
    | <:expr< $int:i$ >>      ->
      let n = <:expr< Value.inj (Value.of_string $str:i$) >> in
      <:expr< const $n$ >>

    | <:expr< $lid:ident$ >> ->
      let x = <:expr< Var.inj (Var.of_string $str:ident$) >> in
      <:expr< var $x$ >>

    | <:expr< $lid:ident$ $e1$ $e2$ >> ->
      (match ident with
        | "+" | "*" | "=" | "!=" | "<" | "<=" | ">" | ">=" ->
          let op = <:expr< Var.inj (Var.of_string $str:ident$) >> in
          <:expr< binop $op$ $s e1$ $s e2$ >>
        (* | ":=" -> *)
          (* let op = <:expr< Var.inj (Var.of_string $str:ident$) >> in *)
          (* <:expr< asgn $s e1$ $s e2$ >> *)
        | _ -> expr
      )
    | _ ->
        Stdpp.raise_with_loc loc
          (Failure
             "syntax not supported due to the \
              lack of Camlp5 documentation")

EXTEND
  GLOBAL: expr;

  expr: LEVEL "expr1" [
    [ "cppmem"; "{"; e = expr; "}" -> subst_cppmem e ]
  ];

END;

(* EXTEND
  GLOBAL: expr;

  expr: LEVEL "expr1" [
    [ "skip" ->
      let fuck = <:expr< Value.inj (Value.of_string "1") >> in
      <:expr< const $fuck$ >>
    ]
    (* | *)
    (* [ "defer"; subj=expr LEVEL "." ->
      <:expr< delay (fun () -> $subj$) >>
    ] *)
  ];

END; *)
