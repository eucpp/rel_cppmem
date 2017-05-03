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
let label     = '?'(digit+ as label_n)

let mo = "sc"|"acq"|"rel"|"relAcq"|"con"|"rlx"|"na" as mo_lxm

rule token = parse
    [' ' '\t' '\n']+    { token lexbuf }     (* skip blanks *)
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | ":="                { ASSIGN }
  | "!="                { NEQ }
  | '='                 { EQ }
  | "<="                { LE }
  | '<'                 { LT }
  | ">="                { GE }
  | '>'                 { GT }
  | "ret"               { RET }
  | "skip"              { SKIP }
  | "stuck"             { STUCK }
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
  | label               { LABEL(int_of_string label_n) }
  | mo                  { MO(Value (MemOrder.of_string mo_lxm)) }
  | var                 { VAR(Value var_lxm) }
  | loc                 { LOC(Value loc_lxm) }
  | integer             { INT(Value.to_logic (Value.of_string int_lxm)) }
  | eof                 { EOF }
