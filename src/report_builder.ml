open Report
open ExtList

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

let col_order_asc ()  = Some(ASC)

let col_order_desc () = Some(DESC)

let with_column cattr report = 
    let col = List.fold_left (fun c f -> f c)
                             { col_name  = None;
                               col_alias = None;
                               col_order = None;
                               col_source = COLUMN("","")
                              }
                              cattr
    in { report with columns = col  :: report.columns }

let build_report e  = 
    let rep = { columns = []; 
                datasources = [];
                connections = []
              }
    in List.fold_left (fun r f -> f r) rep e 


