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
let number  = '-'? digit+ '.' digit+
let space   = [' ' '\t']

rule token = parse
    | "#"  [^'\n']* '\n' { incr_linenum lexbuf; token lexbuf }
    | "%%" [^'\n']* '\n' { incr_linenum lexbuf; token lexbuf }
    | space           { token lexbuf }
    | '\n'            { incr_linenum lexbuf; token lexbuf }
    | decimal         { INT (int_of_string(lexeme lexbuf)) }
    | hexadecimal     { INT (int_of_string(lexeme lexbuf)) }
    | number          { NUMBER (lexeme lexbuf) }
    | "ALIAS"         { ALIAS }
    | "COLUMN"        { COLUMN }
    | "END"           { END }
    | "FIELD"         { FIELD }
    | "FILTER"        { FILTER }
    | "FOLD"          { FOLD }
    | "NAME"          { NAME }
    | "GROUP"         { GROUP }
    | "SORT"          { SORT }
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
    | "OUTPUT"        { OUTPUT }
    | "STDOUT"        { STDOUT }
    | "FILE"          { FILE }
    | "TEMPORARY"     { TEMPORARY }
    | "POSTPROCESS"   { POSTPROCESS }
    | "ECHO"          { ECHO }
    | "ABORT"         { ABORT }
    | "BEFORE"        { BEFORE }
    | "AFTER"         { AFTER }
    | "."             { DOT }
    | "EQ"            { EQ }
    | "NE"            { NE }
    | "LT"            { LT }
    | "GT"            { GT }
    | "LE"            { LE }
    | "GE"            { GE }
    | "OR"            { OR }
    | "AND"           { AND }
    | "IN"            { IN }
    | "LIKE"          { LIKE }
    | "BETWEEN"       { BETWEEN }
    | "("             { OBR }
    | ")"             { CBR }
    | ident           { IDENT (lexeme lexbuf) }

    (* Char literals *)
    | '\'' ([^'\'']+ as c) '\'' { INT ( Char.code ((unescape c).[0]) ) }

    | '\"' (([^'\"']|'\\' '"')* as s) '\"' { STRING((unescape s)) }
    | eof             { EOF }

{
}

