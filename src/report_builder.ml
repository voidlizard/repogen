open Report
open ExtList
open ExtString

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

let with_col_order ord col = { col with col_order = ord }

let with_group col = { col with col_group = true }

let col_order_asc ()  = Some(ASC)

let col_order_desc () = Some(DESC)

let with_column cattr report = 
    let col = List.fold_left (fun c f -> f c)
                             { col_name  = None;
                               col_alias = None;
                               col_order = None;
                               col_source = COLUMN("","");
                               col_group = false
                              }
                              cattr
    in { report with columns = col  :: report.columns }

let with_template tpl report = { report with template = Some(tpl) }

let with_template_dirs d report = 
    { report with template_dirs = report.template_dirs @ List.map String.strip (String.nsplit d ":") }

let with_output_stdout () report = 
    { report with output = STDOUT }

let with_output_file f report = 
    { report with output = FILE(f) }

let with_output_temp ?prefix:(p="repogen") ?suffix:(s=".out") () report = 
    { report with output = FILE((Filename.temp_file p s)) }

let build_report e  = 
    let rep = { columns = []; 
                datasources = [];
                connections = [];
                template = None;
                template_dirs = [""; "."];
                output = STDOUT
              }
    in let r = List.fold_left (fun r f -> f r) rep e 
    in normalize_report { r with columns = List.rev r.columns }


