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

    in let rep = { columns = [ 
                               { col_name   = Some("водитель");
                                 col_alias  = Some("drv");
                                 col_order  = None;
                                 col_source = COLUMN("vbus", "driver")
                               }; 
                               { col_name  = Some("госномер");
                                 col_alias = Some("numb");
                                 col_order = Some(ASC);
                                 col_source = COLUMN("vbus","gos_number")
                               }; 
                               { col_name  = None;
                                 col_alias = None;
                                 col_order = Some(ASC);
                                 col_source = COLUMN("vbus","field")
                               }; 
                             ];
                   datasources = [("vbus", DS_TABLE("vbus"))];
                   connections = [("mososm2", "dbname = mososm2")]
                 }

    in print_endline (sql_of rep)
