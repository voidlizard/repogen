open Postgresql
open Parser
open Util

open Report

let parse_channel ch =
  let lex = Message.lexer_from_channel "stdin" ch
  in let ast = Parser.toplevel Lexer.token lex
  in ast

let () =
    let report = parse_channel (Pervasives.stdin)

    in let _ = List.iter (fun (a, DS_TABLE(n)) -> Printf.printf "%s %s\n" a n) report.datasources
    in let _ = List.iter (fun (a, b) -> Printf.printf "%s %s\n" a b) report.connections
    in print_endline (sql_of report)
