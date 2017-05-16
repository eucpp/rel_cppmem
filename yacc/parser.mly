%{
  open Lang.Term.T
  open MiniKanren
%}

%token <Memory.Var.tl> VAR
%token <Memory.Loc.tl> LOC
%token <int> LABEL

%token <Memory.Value.tl> INT
%token <Memory.MemOrder.tl> MO

%token PLUS MINUS TIMES
%token EQ NEQ LT LE GT GE
%token RET
%token SKIP STUCK
%token ASSIGN
%token SEMICOLON UNDERSCORE COMMA
%token BOPEN BCLOSE
%token IF THEN ELSE FI
%token REPEAT END
%token SPW TOPEN TSEP TCLOSE
%token EOF

%left PLUS MINUS TIMES
%right SEMICOLON COMMA

%start parse
%type <Lang.Term.tl> parse

%%

parse:
  | s = stmt; EOF
    { s }
;
;
stmt:
  | RET; e = expr
    { e }
  | IF; e = expr; THEN; s1 = stmt; ELSE; s2 = stmt; FI
    { Value (If (e, s1, s2)) }
  | REPEAT e = expr END
    { Value (Repeat e) }
  | e1 = expr; ASSIGN; e2 = expr
    { Value (Asgn (e1, e2)) }
  | l = LOC; UNDERSCORE; m = MO; ASSIGN; e = expr
    { Value (Write (m, l, e)) }
  | s1 = stmt; SEMICOLON; s2 = stmt
    { Value (Seq (s1, s2)) }
  | SPW; TOPEN; s1 = stmt; TSEP; s2 = stmt; TCLOSE
    { Value (Spw (s1, s2)) }
  | SKIP
    { Value Skip }
  | STUCK
    { Value Stuck }
  | label = LABEL
    { Var (label, []) }
;
expr:
  | n = INT
    { Value (Const n) }
  | v = VAR
    { Value (Var v) }
  | l = LOC; UNDERSCORE; m = mo
    { Value (Read (m, l)) }
  | BOPEN; e = expr; BCLOSE
    { e }
  | e1 = expr; PLUS; e2 = expr
    { Value (Binop ((Value "+"), e1, e2)) }
  | e1 = expr; MINUS; e2 = expr
    { Value (Binop ((Value "-"), e1, e2)) }
  | e1 = expr; TIMES; e2 = expr
    { Value (Binop ((Value "*"), e1, e2)) }
  | e1 = expr; EQ; e2 = expr
    { Value (Binop ((Value "="), e1, e2)) }
  | e1 = expr; NEQ; e2 = expr
    { Value (Binop ((Value "!="), e1, e2)) }
  | e1 = expr; LT; e2 = expr
    { Value (Binop ((Value "<"), e1, e2)) }
  | e1 = expr; LE; e2 = expr
    { Value (Binop ((Value "<="), e1, e2)) }
  | e1 = expr; GT; e2 = expr
    { Value (Binop ((Value ">"), e1, e2)) }
  | e1 = expr; GE; e2 = expr
    { Value (Binop ((Value ">="), e1, e2)) }
  | e1 = expr; COMMA; e2 = expr
    { Value (Pair (e1, e2)) }
  | label = LABEL
    { Var (label, []) }
;
mo:
  | m = MO
    { m }
  | label = LABEL
    { Var (label, []) }
%%
