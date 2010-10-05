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
    in let _ = print_endline (Ast.dump report)

    in let rep = { columns = [ 
                               { col_name  = Some("водитель");
                                 col_alias = Some("drv");
                                 col_order = None
                               }; 
                               { col_name  = Some("госномер");
                                 col_alias = Some("numb");
                                 col_order = Some(ASC)
                               }; 
                               { col_name  = None;
                                 col_alias = None;
                                 col_order = Some(ASC)
                               }; 
                             ]
                 }

    in print_endline (sql_of rep)
