#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Pcaml
open Printf
open Lang.Term

EXTEND
  GLOBAL: expr;

  expr: LEVEL "expr1" [
    [ "skip_me" ->
      <:expr< skip () >>
    ]
    (* | *)
    (* [ "defer"; subj=expr LEVEL "." ->
      <:expr< delay (fun () -> $subj$) >>
    ] *)
  ];

END;
