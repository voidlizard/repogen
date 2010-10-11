{
open ExtString  
open ExtString

}

let ident   = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*


rule process result subst  = parse
  | '$' '{' (ident as idnt) '}' { process ((try List.assoc idnt subst with Not_found -> "") :: result)
                                           subst lexbuf 
                                }
  | _     {process ((Lexing.lexeme lexbuf) :: result) subst lexbuf}
  | eof   { List.rev result }

{

let parse_string s repl = 
    let lexbuf = Lexing.from_string s
    in String.join "" (process [] repl lexbuf)

}

