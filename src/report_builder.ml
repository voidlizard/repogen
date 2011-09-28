open Report
open ExtList
open ExtString

module R = Report

type ds_tbl_def = { ds_alias: string; ds_src: string }

type ds_fun_def = { ds_fun_alias: string; ds_fun_call: fun_call_t }

let datasource_source source ds = { ds with ds_src = source }

let datasource_alias alias ds   = { ds with ds_alias = alias }

let datasource_fun_alias alias ds   = { ds with ds_fun_alias = alias }

let datasource_fun_source source ds = { ds with ds_fun_call = source }

let with_datasource_table ds_f report =
    let mt_ds = { ds_alias = ""; ds_src = "" }
    in let ds = List.fold_left (fun ds f -> f ds) mt_ds ds_f
    in { report with datasources = (ds.ds_alias, DS_TABLE(ds.ds_src)) :: report.datasources }

let with_datasource_function ds_f report =
    let mt_ds = { ds_fun_alias = ""; ds_fun_call = {fun_ns=SQL; fun_name="fake"; fun_args = []}}
    in let ds = List.fold_left (fun ds f -> f ds) mt_ds ds_f
    in { report with datasources = (ds.ds_fun_alias, DS_FUN(ds.ds_fun_call)) :: report.datasources }

let with_datasource ds report = ds report

let with_connection conn report =
    { report with connections =  conn :: report.connections }

let connection_arg_str s = CONN_STR_CONST(s)

let connection_arg_var s = CONN_VAR_REF(s)

let col_ref a b = COLUMN(a,b)

let table_ref a = COLUMN(a, "*")

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

let col_fun_call x = FUN_CALL(x)

let string_constant s = STR_CONST(s)

let number_constant v = NUM_CONST(v)

let var_ref v = VAR_REF(v)

let var_filt_arg x = SRC x

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
    in { report with columns = col :: report.columns }

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
        let cmd = Stmpl.subst s (R.metavars r)
        in let _ = print_endline cmd
        in Unix.system cmd; r

    in { report with post_actions = pp :: report.post_actions }

let with_postprocess_drop () report = 
    { report with post_actions = [] }

let with_echo w s r =
    let mv = R.metavars 
    in let echo = (fun r' -> print_endline (Stmpl.subst s (mv r')); r')
    in match w with 
    | R.BEFORE -> { r with pre_actions  = echo :: r.pre_actions  } 
    | R.AFTER  -> { r with post_actions = echo :: r.post_actions  } 

let with_abort w r = 
    let abrt = (fun r' -> failwith "ABORTED")
    in match w with
    | R.BEFORE -> { r with pre_actions  = abrt :: r.pre_actions }
    | R.AFTER  -> { r with post_actions = abrt :: r.post_actions }

let fun_arg_ident i = FA_ALIAS(i)

let fun_arg_col_ref r = FA_SRC(r) 

let fun_arg_table_ref r = FA_SRC(r)

let fun_arg_src s = FA_SRC(FUN_CALL(s))

let fun_arg_num num = FA_VAL(NUM_CONST(num))

let fun_arg_str s = FA_VAL(STR_CONST(s))

let fun_arg_var v = FA_VAL(v)

let fun_call (ns, (name, args)) = 
    {fun_ns = ns; fun_name = name; fun_args = args}

type tmp_fld = { tmp_alias: string option; tmp_src: field_src_t option; tmp_flt: (filt_op_t * string) list }

let with_field_source src field = 
    { field with tmp_src = Some(FIELD_FUN_CALL(src)) }

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

type tmp_vardef = { tmp_var_name: string option; tmp_var_alias: string option; tmp_var_value: string option }

let tmp_var = { tmp_var_name = None; tmp_var_alias = None; tmp_var_value = None }

let with_tmp_var_name  name  v = { v with tmp_var_name = Some(name) }
let with_tmp_var_alias alias v = { v with tmp_var_alias = Some(alias) }
let with_tmp_var_value value v = { v with tmp_var_value = Some(value) }

let assemble_var = List.fold_left (fun acc f -> f acc) tmp_var

let with_var_def var r =
    match var with 
     | { tmp_var_alias = Some(alias); tmp_var_value=Some(value); tmp_var_name=Some(name) } -> { r with vars = (alias, fun _ -> value) :: r.vars;
                                                                                                 var_desc = (alias, name) :: r.var_desc }
     | { tmp_var_alias = Some(alias); tmp_var_value=Some(value) } -> { r with vars = (alias, fun _ -> value) :: r.vars }
     | _ -> failwith "Variable's alias and value are mandatory"

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
                var_desc = [];
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


