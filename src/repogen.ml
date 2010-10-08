open Postgresql
open Parser
open Util

open Report
open Report_model

module Db = Db_pg
module T = Templating.Templating


open Printf

let parse_channel ch =
  let lex = Message.lexer_from_channel "stdin" ch
  in let ast = Parser.toplevel Lexer.token lex
  in ast


let list_of_ds report ds =
    let hdr = (column_headers report)
    in List.map ( fun x -> List.map2 (fun (a,_) v -> (a,v)) hdr x) ds

let () =
    let report = parse_channel (Pervasives.stdin)
    
    in let _ = List.iter (fun (a, DS_TABLE(n)) -> Printf.printf "%s %s\n" a n) report.datasources
    in let _ = List.iter (fun (a, b) -> Printf.printf "%s %s\n" a b) report.connections
    in let sql = sql_of report
    in let _ = print_endline sql
   
    in let cache = T.cache ()
    in let tmpl  = T.from_file cache "hello.tmpl"

    in 
        try
            let data = Db.with_connection (fun conn -> Db.select_all conn sql (fun ds -> list_of_ds report ds))
                                                                              (connection_of report)
            in let model = Model.make (column_headers report) data [("message", "HELLO!")]
            in let s = T.render_string tmpl model 
            in let () = print_endline s
            in ()
        with Error e -> prerr_endline (string_of_error e)
             | e     -> prerr_endline (Printexc.to_string e)

