
{
open ExtString  
open ExtString

}

let ident   = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule kv_parser = parse
    | (ident as k) '=' (_+ as v)  { Some(k,v) }
    | _                           { None }


{
let parse_kv s = 
    let lexbuf = Lexing.from_string s
    in (kv_parser lexbuf)
}

