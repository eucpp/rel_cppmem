{
    open MiniKanren
    open Parser        (* The type token is defined in parser.mli *)
    open Lang
}

let lc = ['a' - 'z']
let uc = ['A' - 'Z']

let digit = ['0' - '9']

let integer   = digit+ as int_lxm
let loc       = lc(lc|digit)* as loc_lxm
let var       = 'r'(lc|digit)* as var_lxm
let label     = 'p'|'q'|'r'|'s'|'t' as label_lxm

let mo = "sc"|"acq"|"rel"|"relAcq"|"con"|"rlx"|"na" as mo_lxm

rule token = parse
    [' ' '\t' '\n']+    { token lexbuf }     (* skip blanks *)
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | "ret"               { RET }
  | "skip"              { SKIP }
  | "stuck"             { STUCK }
  | ":="                { ASSIGN }
  | ';'                 { SEMICOLON }
  | '_'                 { UNDERSCORE }
  | "if"                { IF }
  | "then"              { THEN }
  | "else"              { ELSE }
  | "fi"                { FI }
  | "repeat"            { REPEAT }
  | "end"               { END }
  | "spw"               { SPW }
  | "{{{"               { TOPEN }
  | "|||"               { TSEP }
  | "}}}"               { TCLOSE }
  | "<?"                { HOPEN }
  | ">"                 { HCLOSE }
  | '?'                 { QUESTION_MARK }
  | mo                  { MO(!!(MemOrder.of_string mo_lxm)) }
  | label               { LABEL(label_lxm) }
  | var                 { VAR(!!var_lxm) }
  | loc                 { LOC(!!loc_lxm) }
  | integer             { INT(Nat.inj (Value.of_string int_lxm)) }
  | eof                 { EOF }
