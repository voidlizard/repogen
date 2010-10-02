open Postgresql
open Parser
open Util

let parse_channel ch =
  let lex = Message.lexer_from_channel "stdin" ch
  in let ast = Parser.toplevel Lexer.token lex
  in ast

let () =
  let _ = parse_channel (Pervasives.stdin)
  in ()
