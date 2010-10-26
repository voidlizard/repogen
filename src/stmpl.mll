{
open ExtString  
open ExtString

type chink_t = Chunk of string | Var of string

}

let ident   = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule split result = parse
  | '$' '{' (ident as idnt) '}' { split (Var(idnt)::result) lexbuf 
                                }
  | _     {split (Chunk(Lexing.lexeme lexbuf)::result) lexbuf}
  | eof   { List.rev result }

{

let subst s repl =
    let sf x = (try List.assoc x repl with Not_found -> "")
    in let v = split [] (Lexing.from_string s)
    in String.join "" (List.map (function Chunk(s) -> s | Var(s) -> sf s) v)


let vars s = 
    let v = split [] (Lexing.from_string s)
    in List.fold_left (fun acc v -> match v with Var(s) -> s :: acc | _ -> acc)
                      [] v

}


