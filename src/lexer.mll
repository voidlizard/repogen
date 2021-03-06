{
  open Parser
  open Lexing
  open Escaping

  let incr_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.pos_lnum + 1;
    Lexing.pos_bol = pos.pos_cnum;
    }

}

let ident   = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let digit   = ['0' - '9']
let decimal = digit+
let hexdigit = ['0' - '9' 'a' - 'f' 'A' - 'F']
let hexadecimal = ("0x" | "0X") hexdigit hexdigit*
let number  = '-'? digit+ ('.' digit+)?

let space   = [' ' '\t' '\r' '\n']

rule token = parse
    | "#"  [^'\n']* '\n' { incr_linenum lexbuf; token lexbuf }
    | "%%" [^'\n']* '\n' { incr_linenum lexbuf; token lexbuf }
    | space           { token lexbuf }
    | '\n'            { incr_linenum lexbuf; token lexbuf }
    | number          { NUMBER (lexeme lexbuf) }
    | "*"             { ASTERISK }
    | ':'             { COLON }
    | "VARIABLE"      { VARIABLE }
    | "VALUE"         { VALUE }
    | "AS"            { AS }
    | "SET"           { SET }
    | "ALIAS"         { ALIAS }
    | "COLUMN"        { COLUMN }
    | "END"           { END }
    | "FIELD"         { FIELD }
    | "FILTER"        { FILTER }
    | "FOLD"          { FOLD }
    | "NAME"          { NAME }
    | "GROUP"         { GROUP }
    | "SORT"          { SORT }
    | "NULL"          { NULL }
    | "NULLS"         { NULLS }
    | "FIRST"         { FIRST }
    | "LAST"          { LAST }
    | "SOURCE"        { SOURCE }
    | "YES"           { YES }
    | "NO"            { NO }
    | "ASC"           { ASC }
    | "DESC"          { DESC }
    | "NONE"          { NONE }
    | "TEMPLATE"      { TEMPLATE }
    | "TEMPLATE_DIRS" { TEMPLATE_DIRS }
    | "CONNECTION"    { CONNECTION }
    | "DATASOURCE"    { DATASOURCE }
    | "TABLE"         { TABLE }
    | "FUNCTION"      { FUNCTION }
    | "OUTPUT"        { OUTPUT }
    | "STDOUT"        { STDOUT }
    | "FILE"          { FILE }
    | "TEMPORARY"     { TEMPORARY }
    | "POSTPROCESS"   { POSTPROCESS }
    | "ECHO"          { ECHO }
    | "ABORT"         { ABORT }
    | "BEFORE"        { BEFORE }
    | "AFTER"         { AFTER }
    | "EQ"            { EQ }
    | "NE"            { NE }
    | "LT"            { LT }
    | "GT"            { GT }
    | "LE"            { LE }
    | "GE"            { GE }
    | "IN"            { IN }
    | "LIKE"          { LIKE }
    | "BY"            { BY }
    | "BETWEEN"       { BETWEEN }
    | "OR"            { OR }
    | "AND"           { AND }
    | "NOT"           { NOT }
    | "SQL"           { SQL }
    | "DROP"          { DROP }
    | ","             { COMMA } 
    | "."             { DOT }
    | "("             { OBR }
    | ")"             { CBR }
    | "{"             { OBRACE }
    | "}"             { CBRACE }
    | "$"             { DOLLAR }
    | ident           { IDENT (lexeme lexbuf) }

    (* Char literals *)
    | '\'' ([^'\'']+ as c) '\'' { INT ( Char.code ((unescape c).[0]) ) }

    | '\"' (([^'\"']|'\\' '"')* as s) '\"' { STRING((unescape s)) }
    | eof             { EOF }

{
}

