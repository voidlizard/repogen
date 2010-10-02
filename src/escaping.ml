open ExtString
open Printf

let unescape s =
    let chars = String.explode s
    in let parse_hex a b = Char.chr (int_of_string (sprintf "0x%c%c" a b))
    in let rec parse s = match s with
        | '\\' :: 'r' :: rest -> '\r' :: parse rest
        | '\\' :: 'n' :: rest -> '\n' :: parse rest
        | '\\' :: 't' :: rest -> '\t' :: parse rest
        | '\\' :: 'b' :: rest -> '\b' :: parse rest
        | '\\' :: '"' :: rest -> '"'  :: parse rest
        | '\\' :: 'x' :: a :: b :: rest -> parse_hex a b :: parse rest
        | '\\' :: 'X' :: a :: b :: rest -> parse_hex a b :: parse rest
        | x  :: rest          -> x :: parse rest 
        | []                  -> []
    in String.implode (parse chars)

