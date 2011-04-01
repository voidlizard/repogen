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
             mutable opt_tmpl: string option;
             mutable opt_vars: (string * string) list
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

   in let vars = List.map (fun (k,v) -> (fun r -> {r with vars = (k, (fun r -> v)) :: r.vars }))
                          opts.opt_vars

   in let fns = 
        out :: tmpl :: vars

   in List.fold_left (fun acc f -> f acc) rep fns

let simple_sql_query report connection sql binds =
    let mutate ds = list_of_ds report ds
    in let fn conn = Db.select_all conn sql mutate ~bind:binds
    in let rows = Db.with_connection fn connection
    in (rows, [])

let multi_sql_query report connection sql binds =
    let mutate ds = list_of_ds report ds
    in let mutate_fields ds = 
        let hdr = fields_of report
        in List.map ( fun x -> List.map2 (fun a v -> (a,v)) hdr x) ds
    in let fn conn = Db.with_block conn (fun () -> let tmp = Db.temp_table conn sql binds
                                                   in let rows = Db.select_all conn (P.sprintf "SELECT * FROM %s" tmp) mutate
                                                   in let (fsql, binds') = parametrized (sql_of_fields report tmp) report
                                                   in let fields' = Db.select_all conn fsql mutate_fields ~bind:(binds') (* FIXME: FETCH ONE ROW *)
                                                   in let fields = try List.hd fields' with ExtList.List.Empty_list -> []
                                                   in (rows, fields)
                                         )
    in Db.with_connection fn connection

let () =
    let opts = { opt_filename = None; opt_output = None; opt_tmpl = None; opt_vars = [] }
    in let _ = Arg.parse [
                            ("--out-file",   Arg.String(fun s -> opts.opt_output <- Some(FILE(s))) , "set output file name");
                            ("--out-stdout", Arg.Unit(fun ()  -> opts.opt_output <- Some(STDOUT) ) , "set output file to STDOUT");
                            ("--template",   Arg.String(fun s -> opts.opt_tmpl <- Some(s) ) , "set template");
                            ("--version",    Arg.Unit(fun () -> print_endline Version.version_string; Pervasives.exit 0), "");
                            ("--define",     Arg.String(fun s -> Option.may (fun x -> opts.opt_vars <- (x::opts.opt_vars) ) (Misc_parsers.parse_kv s) ) , "NAME=VALUE, define a variable");
                         ] (fun x -> opts.opt_filename <- Some(x) ) (P.sprintf "%s\nUsage:" Version.version_string)

    in let report' = with_options opts (report_of opts)

    in let cache = T.cache ()

    in 
        try
            let report = execute_actions BEFORE report'

            in let (sql, binds) = parametrized (sql_of report) report
 
            in let conn = connection_of report

            in let (data, fields) = if not (has_sql_fields report)
                                    then simple_sql_query report conn sql binds
                                    else multi_sql_query report conn sql binds
            
            in let model = Model.make (column_headers report) data ((metavars report) @ fields)

            in match report.template with 
               | Some(s) ->
                   let tmpl  = T.from_file cache s
                   in let () = dump_output report (T.render_string tmpl model)
                   in ignore (execute_actions AFTER report)
               | None -> ()
            
        with Error e -> prerr_endline (string_of_error e)
             | e     -> prerr_endline (Printexc.to_string e)

