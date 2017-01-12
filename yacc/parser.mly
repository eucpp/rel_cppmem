%token <string> VAR
%token <string> LOC
%token <Memory.mem_order> MO
%token PLUS MINUS TIMES
%token RET STUCK
%token ASSIGN 
%token SEMICOLON UNDERSCORE
%token IF THEN ELSE
%token REPEAT END
%token UNDERSCORE

%left PLUS MINUS
%left TIMES

%start expr
%type <Lang.ExprTerm> expr

%start stmt
%type <Lang.StmtTerm> stmt

%%

stmt:
   RET expr                             {Lang.StmtTerm.AExpr $2}
 | STUCK                                {Lang.StmtTerm.Stuck}
 | IF stmt THEN stmt ELSE stmt          {Lang.StmtTerm.If ($2, $4, $6)}
 | REPEAT stmt END                      {Lang.StmtTerm.Repeat $2}
 | VAR ASSIGN stmt                      {Lang.StmtTerm.Asgn ($1, $3)}
 | LOC UNDERSCORE MO ASSIGN expr        {Lang.StmtTerm.Write ($3, $1, $5)}
 | LOC UNDERSCORE MO                    {Lang.StmtTerm.Read ($3, $1)}
 | stmt SEMICOLON stmt                  {Lang.StmtTerm.Seq ($1, $3)}

%%