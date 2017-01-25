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
%type <Lang.StmtTerm.t> main

%%

main:
    stmt EOF                             { $1 }
;
stmt:
    RET expr                             { Lang.StmtTerm.AExpr $2 }
  | STUCK                                { Lang.StmtTerm.Stuck }
  | IF expr THEN stmt ELSE stmt FI       { Lang.StmtTerm.If (Lang.StmtTerm.AExpr $2, $4, $6) }
  | REPEAT stmt END                      { Lang.StmtTerm.Repeat $2 }
  | VAR ASSIGN expr                      { Lang.StmtTerm.Asgn (Lang.StmtTerm.AExpr (Lang.ExprTerm.Var $1), Lang.StmtTerm.AExpr $3) }
  | LOC UNDERSCORE MO ASSIGN expr        { Lang.StmtTerm.Write ($3, $1, $5) }
  | LOC UNDERSCORE MO                    { Lang.StmtTerm.Read ($3, $1) }
  | stmt SEMICOLON stmt                  { Lang.StmtTerm.Seq ($1, $3) }
  | SKIP                                 { Lang.StmtTerm.Skip }
  | STUCK                                { Lang.StmtTerm.Stuck }
;
expr:
    INT                                  { Lang.ExprTerm.Const $1 } 
  | VAR                                  { Lang.ExprTerm.Var $1 }
  | expr PLUS expr                       { Lang.ExprTerm.Binop ("+", $1, $3) }
  | expr MINUS expr                      { Lang.ExprTerm.Binop ("-", $1, $3) }
  | expr TIMES expr                      { Lang.ExprTerm.Binop ("*", $1, $3) }
;
%%