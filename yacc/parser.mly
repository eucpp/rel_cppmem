%{
  open Lang.Term.T
  open MiniKanren
%}

%token <Lang.Var.tl> VAR
%token <Lang.Loc.tl> LOC
%token <int> LABEL

%token <Lang.Value.tl> INT
%token <Lang.MemOrder.tl> MO

%token PLUS MINUS TIMES
%token RET
%token SKIP STUCK
%token ASSIGN
%token SEMICOLON UNDERSCORE
%token IF THEN ELSE FI
%token REPEAT END
%token SPW TOPEN TSEP TCLOSE
%token HOPEN HCLOSE QUESTION_MARK
%token EOF

%left PLUS MINUS
%left TIMES
%right SEMICOLON

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
  | v = VAR; ASSIGN; e = expr
    { Value (Asgn ((Value (Var v)), e)) }
  | l = LOC; UNDERSCORE; mo = MO; ASSIGN; e = expr
    { Value (Write (mo, l, e)) }
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
  | l = LOC; UNDERSCORE; mo = MO
    { Value (Read (mo, l)) }
  | e1 = expr; PLUS; e2 = expr
    { Value (Binop ((Value "+"), e1, e2)) }
  | e1 = expr; MINUS; e2 = expr
    { Value (Binop ((Value "-"), e1, e2)) }
  | e1 = expr; TIMES; e2 = expr
    { Value (Binop ((Value "*"), e1, e2)) }
  | label = LABEL
    { Var (label, []) }
;
%%
