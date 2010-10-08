open Postgresql
open Parser
open Util

open Report

module Db = Db_pg

let parse_channel ch =
  let lex = Message.lexer_from_channel "stdin" ch
  in let ast = Parser.toplevel Lexer.token lex
  in ast

let print_row r = Array.iter ( fun x -> Printf.printf "'%s'\t" x) r; print_endline "" 

let print_rows rows  = 
    Array.iter print_row rows

let () =
    let report = parse_channel (Pervasives.stdin)

    in let _ = List.iter (fun (a, DS_TABLE(n)) -> Printf.printf "%s %s\n" a n) report.datasources
    in let _ = List.iter (fun (a, b) -> Printf.printf "%s %s\n" a b) report.connections
    in let sql = sql_of report
    in let _ = print_endline sql
    in 
        try
            Db.with_connection (fun conn -> ignore (Db.select_all conn sql print_rows)) (connection_of report)
        with Error e -> prerr_endline (string_of_error e)
             | e     -> prerr_endline (Printexc.to_string e)

