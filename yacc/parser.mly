%token <string> VAR
%token <string> LOC
%token <int> INT
%token <Memory.mem_order> MO
%token PLUS MINUS TIMES
%token RET 
%token SKIP STUCK
%token ASSIGN 
%token SEMICOLON UNDERSCORE
%token IF THEN ELSE FI
%token REPEAT END
%token EOF

%left PLUS MINUS
%left TIMES

%start main
%type <Lang.Term.t> main

%%

main:
    stmt EOF                             { $1 }
;
stmt:
    RET expr                             { $2 }
  | STUCK                                { Lang.Term.Stuck }
  | IF expr THEN stmt ELSE stmt FI       { Lang.Term.If ($2, $4, $6) }
  | REPEAT stmt END                      { Lang.Term.Repeat $2 }
  | VAR ASSIGN expr                      { Lang.Term.Asgn (Lang.Term.Var $1, $3) }
  | LOC UNDERSCORE MO ASSIGN expr        { Lang.Term.Write ($3, $1, $5) }
  | LOC UNDERSCORE MO                    { Lang.Term.Read ($3, $1) }
  | stmt SEMICOLON stmt                  { Lang.Term.Seq ($1, $3) }
  | SKIP                                 { Lang.Term.Skip }
  | STUCK                                { Lang.Term.Stuck }
;
expr:
    INT                                  { Lang.Term.Const $1 } 
  | VAR                                  { Lang.Term.Var $1 }
  | expr PLUS expr                       { Lang.Term.Binop ("+", $1, $3) }
  | expr MINUS expr                      { Lang.Term.Binop ("-", $1, $3) }
  | expr TIMES expr                      { Lang.Term.Binop ("*", $1, $3) }
;
%%