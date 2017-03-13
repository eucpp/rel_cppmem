{
    open Parser        (* The type token is defined in parser.mli *)
}

let lc = ['a' - 'z']
let uc = ['A' - 'Z']

let digit = ['0' - '9']

let integer    = digit+ as int_lxm
let loc = lc(lc|digit)* as loc_lxm
let var = 'r'(lc|digit)* as var_lxm

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
  | mo                  { MO(Lang.mo_of_string mo_lxm) }
  | var                 { VAR(var_lxm) }
  | loc                 { LOC(loc_lxm) }
  | integer             { INT(int_of_string int_lxm) }
  | eof                 { EOF }
