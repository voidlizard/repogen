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

let dump_output report out = 
    match report.output with
    | STDOUT    -> output_string Pervasives.stdout out
    | FILE(s)   -> Std.output_file s out

let () =
    let report = parse_channel (Pervasives.stdin)

    in let cache = T.cache ()

    in 
        try
            let report = execute_actions BEFORE report
            
            in let sql = sql_of report
            
            in let data = Db.with_connection (fun conn -> Db.select_all conn sql (fun ds -> list_of_ds report ds))
                                                                              (connection_of report)
            in let model = Model.make (column_headers report) data ([("SQL", sql)] @ (metavars report))

            in match report.template with 
               | Some(s) ->
                   let tmpl  = T.from_file cache s
                   in let () = dump_output report (T.render_string tmpl model)
                   in ignore (execute_actions AFTER report)
               | None -> ()
            
        with Error e -> prerr_endline (string_of_error e)
             | e     -> prerr_endline (Printexc.to_string e)

