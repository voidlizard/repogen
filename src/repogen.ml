open Postgresql
open Parser
open Util

open Report
open Report_model

module Db = Db_pg
module T = Templating.Templating
module B = Report_builder

open Printf

let parse_channel ch =
  let lex = Message.lexer_from_channel "stdin" ch
  in let ast = Parser.toplevel Lexer.token lex
  in ast

let parse_string s = 
  let lex = Message.lexer_from_string s ~fn:(Some("STDIN"))
  in let ast = Parser.toplevel Lexer.token lex
  in ast

let list_of_ds report ds =
    let hdr = (column_headers report)
    in List.map ( fun x -> List.map2 (fun (a,_) v -> (a,v)) hdr x) ds

let dump_output report out = 
    match report.output with
    | STDOUT    -> output_string Pervasives.stdout out
    | FILE(s)   -> Std.output_file s out

type opt = { mutable opt_filename: string option;
             mutable opt_output: output_t option;
             mutable opt_tmpl: string option
           }

let report_of = function
    | { opt_filename = None }    -> parse_channel (Pervasives.stdin)
    | { opt_filename = Some(x) } -> parse_string (Std.input_file x)


let with_options opts rep  =
   let out r = match opts.opt_output with
               | None    -> r 
               | Some(x) -> { r with output = x }

   in let tmpl r = match opts.opt_tmpl with
                  | None -> r
                  | Some(x) -> { r with template = Some(x) }

   in let fns = 
        out :: tmpl :: []

   in List.fold_left (fun acc f -> f acc) rep fns


let () =
    let opts = { opt_filename = None; opt_output = None; opt_tmpl = None }
    in let _ = Arg.parse [
                            ("--out-file",   Arg.String(fun s -> opts.opt_output <- Some(FILE(s))) , "set output file name");
                            ("--out-stdout", Arg.Unit(fun ()  -> opts.opt_output <- Some(STDOUT) ) , "set output file to STDOUT");
                            ("--template",   Arg.String(fun s  -> opts.opt_tmpl <- Some(s) ) , "set template");
                            ("--define",     Arg.String(fun s -> ()) , "define variable NAME=VALUE");
                         ] (fun x -> opts.opt_filename <- Some(x) ) "Usage:"


    in let report' = with_options opts (report_of opts)

    in let cache = T.cache ()

    in 
        try
            let report = execute_actions BEFORE report'

            in let (sql, binds) = parametrized_sql report

            in let data = Db.with_connection (fun conn -> Db.select_all conn sql (fun ds -> list_of_ds report ds) ~bind:binds )
                                                                              (connection_of report)
            in let model = Model.make (column_headers report) data (metavars report)

            in match report.template with 
               | Some(s) ->
                   let _ = P.printf "TEMPLATE: %s\n" s 
                   in let tmpl  = T.from_file cache s
                   in let () = dump_output report (T.render_string tmpl model)
                   in ignore (execute_actions AFTER report)
               | None -> ()
            
        with Error e -> prerr_endline (string_of_error e)
             | e     -> prerr_endline (Printexc.to_string e)

