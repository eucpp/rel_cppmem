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

%start expr_main
%type <Lang.ExprTerm.t> expr_main

%start stmt_main
%type <Lang.StmtTerm.t> stmt_main

%%

stmt_main:
    stmt EOF                            { $1 }
;
stmt:
    RET expr                             { Lang.StmtTerm.AExpr $2 }
  | STUCK                                { Lang.StmtTerm.Stuck }
  | IF stmt THEN stmt ELSE stmt FI       { Lang.StmtTerm.If ($2, $4, $6) }
  | REPEAT stmt END                      { Lang.StmtTerm.Repeat $2 }
  | VAR ASSIGN stmt                      { Lang.StmtTerm.Asgn (Lang.StmtTerm.AExpr (Lang.ExprTerm.Var $1), $3) }
  | LOC UNDERSCORE MO ASSIGN expr        { Lang.StmtTerm.Write ($3, $1, $5) }
  | LOC UNDERSCORE MO                    { Lang.StmtTerm.Read ($3, $1) }
  | stmt SEMICOLON stmt                  { Lang.StmtTerm.Seq ($1, $3) }
;
expr_main:
    expr EOF                            { $1 }
;
expr:
    INT                                  { Lang.ExprTerm.Const $1 } 
  | VAR                                  { Lang.ExprTerm.Var $1 }
  | expr PLUS expr                       { Lang.ExprTerm.Binop ("+", $1, $3) }
  | expr MINUS expr                      { Lang.ExprTerm.Binop ("-", $1, $3) }
  | expr TIMES expr                      { Lang.ExprTerm.Binop ("*", $1, $3) }
;
%%