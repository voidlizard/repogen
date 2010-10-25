open Report
open ExtList
open ExtString

module R = Report

type ds_tbl_def = { ds_alias: string; ds_src: string }

let datasource_source source ds = { ds with ds_src = source }

let datasource_alias alias ds   = { ds with ds_alias = alias }

let with_datasource_table ds_f report =
    let mt_ds = { ds_alias = ""; ds_src = "" }
    in let ds = List.fold_left (fun ds f -> f ds) mt_ds ds_f
    in { report with datasources = (ds.ds_alias, DS_TABLE(ds.ds_src)) :: report.datasources }

let with_datasource ds report = ds report

let with_connection conn report =
    { report with connections =  conn :: report.connections }

let col_ref a b = COLUMN(a,b)

let with_col_source cref col = { col with col_source = cref }

let with_col_alias alias col = { col with col_alias = Some(alias) }

let with_col_name name col = { col with col_name = Some(name) }

let with_col_order ord col = 
    List.fold_left ( fun acc f -> f acc ) col ord

let with_group col = { col with col_group = true }

let with_col_filt flt col = { col with col_filter = Some(flt) }

let col_order_asc ()  = (fun c -> { c with col_order = Some(ORDER(ASC, None)) })

let col_order_desc () = (fun c -> { c with col_order = Some(ORDER(DESC, None)) })

let col_nothing () = (fun c -> c)

let col_nulls_first () =
    (fun c -> match c.col_order with 
              | Some(ORDER(x, y)) -> {c with col_order = Some(ORDER(x, Some(NULLS_FIRST)))} 
              | None              -> c )

let col_nulls_last () =
    (fun c -> match c.col_order with 
              | Some(ORDER(x, y)) -> {c with col_order = Some(ORDER(x, Some(NULLS_LAST)))}
              | None              -> c )

let col_fold () = (fun c -> { c with col_fold = true })

let string_constant s = STR_CONST(s)

let number_constant v = NUM_CONST(v)

let with_column cattr report = 
    let col = List.fold_left (fun c f -> f c)
                             { col_name  = None;
                               col_alias = None;
                               col_order = None;
                               col_source = COLUMN("","");
                               col_group = false;
                               col_filter = None;
                               col_fold = false
                              }
                              cattr
    in let (args, col') = extract_query_args col
    in { report with columns = col' :: report.columns;
                     query_args = report.query_args @ args }

let with_template tpl report = { report with template = Some(tpl) }

let with_template_dirs d report = 
    { report with template_dirs = report.template_dirs @ List.map String.strip (String.nsplit d ":") }

let with_output_stdout () report = 
    { report with output = STDOUT }

let with_output_file f report = 
    { report with output = FILE(f) }

let with_output_temp ?prefix:(p="repogen") ?suffix:(s=".out") () report = 
    { report with output = FILE((Filename.temp_file p s)) }

let with_postprocess s report = 

    let pp r =
        let cmd = Stmpl.parse_string s (R.metavars r)
        in let _ = print_endline cmd
        in Unix.system cmd; r

    in { report with post_actions = pp :: report.post_actions }

let with_postprocess_drop () report = 
    { report with post_actions = [] }

let with_echo w s r =
    let mv = R.metavars 
    in let echo = (fun r' -> print_endline (Stmpl.parse_string s (mv r')); r')
    in match w with 
    | R.BEFORE -> { r with pre_actions  = echo :: r.pre_actions  } 
    | R.AFTER  -> { r with post_actions = echo :: r.post_actions  } 

let with_abort w r = 
    let abrt = (fun r' -> failwith "ABORTED")
    in match w with
    | R.BEFORE -> { r with pre_actions  = abrt :: r.pre_actions }
    | R.AFTER  -> { r with post_actions = abrt :: r.post_actions }

let fun_arg_ident i = FA_ALIAS(i)

let fun_call (ns, (name, args)) = 
    FIELD_FUN_CALL({fun_ns = ns; fun_name = name; fun_args = args})

type tmp_fld = { tmp_alias: string option; tmp_src: field_src_t option; tmp_flt: (filt_op_t * string) list }

let with_field_source src field = 
    { field with tmp_src = Some(src) }

let with_field_alias alias field =
    { field with tmp_alias = Some(alias) }

let with_field_filter by flt field =
    { field with tmp_flt = (flt, by) :: field.tmp_flt }

let with_field fattr report = 
    let tmp = List.fold_left (fun acc f -> f acc) { tmp_alias = None;
                                                    tmp_src   = None;
                                                    tmp_flt   = [] 
                                                   } fattr
    in let field = match tmp with
        | { tmp_alias = Some(x); tmp_src = Some(y); tmp_flt = f } -> { field_alias = x; field_source = y; field_flt = f }
        | _                                          -> failwith "Field alias and source are mandatory"
    in { report with fields = field :: report.fields }

let populate_vars report = 
    let v = List.map ( fun (n,v) -> (n, (fun r -> str_of_val (List.assoc n r.query_args)))) report.query_args
    in { report with vars = report.vars @ v}

let build_report e  = 
    let rep = { columns = [];
                fields = [];
                datasources = [];
                connections = [];
                template = None;
                template_dirs = [""; "."];
                output = STDOUT;
                pre_actions = [];
                post_actions = [];
                query_args = [];
                vars =  ("SQL", (fun r -> try sql_of r with _ -> ""))
                     :: ("OUTPUT",   (fun r -> match r.output with STDOUT -> "stdout" | FILE(s) -> s))
                     :: ("TEMPLATE", (fun r -> match r.template with Some(x) -> x | _ -> ""))
                     :: []
              }
    in let r = List.fold_left (fun r f -> f r) rep e 
    in populate_vars ( normalize_report { r with columns = List.rev r.columns;
                                                 pre_actions = List.rev r.pre_actions;
                                                 post_actions = List.rev r.post_actions;
                                        })


