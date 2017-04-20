%{
  open Lang
  open MiniKanren
%}

%token <Lang.Var.ti> VAR
%token <Lang.Loc.ti> LOC
%token <char> LABEL

%token <Lang.Value.ti> INT
%token <Lang.MemOrder.ti> MO

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
%type <Lang.Term.tt> parse

%start parse_partial
%type <Lang.Mapping.t -> Lang.Term.ti> parse_partial

%%

parse:
  | s = stmt; EOF
    { MiniKanren.prj @@ s Mapping.empty }
;
parse_partial:
  | s = stmt; EOF
    { s }
;
stmt:
  | RET; e = expr
    { e }
  | IF; e = expr; THEN; s1 = stmt; ELSE; s2 = stmt; FI
    { fun map -> if' (e map) (s1 map) (s2 map) }
  | REPEAT e = expr END
    { fun map -> repeat (e map) }
  | v = VAR; ASSIGN; e = expr
    { fun map -> asgn (var v) (e map) }
  | l = LOC; UNDERSCORE; mo = MO; ASSIGN; e = expr
    { fun map -> write mo l (e map) }
  | s1 = stmt; SEMICOLON; s2 = stmt
    { fun map -> seq (s1 map) (s2 map) }
  | SPW; TOPEN; s1 = stmt; TSEP; s2 = stmt; TCLOSE
    { fun map -> spw (s1 map) (s2 map) }
  | SKIP
    { fun _ -> skip }
  | STUCK
    { fun _ -> stuck }
  | QUESTION_MARK; lab = LABEL
    { fun map -> Mapping.subst map (Char.escaped lab) }
;
expr:
  | n = INT
    { fun _ -> const n }
  | v = VAR
    { fun _ -> var v }
  | l = LOC; UNDERSCORE; mo = MO
    { fun _ -> read mo l }
  | e1 = expr; PLUS; e2 = expr
    { fun map -> binop !!"+" (e1 map) (e2 map) }
  | e1 = expr; MINUS; e2 = expr
    { fun map -> binop !!"-" (e1 map) (e2 map) }
  | e1 = expr; TIMES; e2 = expr
    { fun map -> binop !!"*" (e1 map) (e2 map) }
  | QUESTION_MARK; lab = LABEL
    { fun map -> Mapping.subst map (Char.escaped lab) }
;
%%
