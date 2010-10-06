open ExtList
open ExtString

type report_t = { columns: col_t list }
and col_t = { col_name: string option;
              col_alias: string option;
              col_order: order_t option;
              col_source: source_t }
and order_t = ASC | DESC
and source_t = COLUMN of string * string

let ident i s = Printf.sprintf "%s %s" i s

let ident_all i s = List.map (fun x -> ident i x) s

let rec normalize_report rep =
    { rep with columns = normalize_columns rep.columns }
and normalize_columns cols = List.mapi normalize_column cols
and normalize_column i  = function ({ col_alias = None } as c)  -> { c with col_alias = Some(Printf.sprintf "col%d" i) }
                                  |({ col_alias = Some _} as c) -> c

let sql_of x =
    let idnt = "   "
    in let i1 = ident idnt
    in let alias x = try Option.get x with Option.No_value -> failwith "Alias is not defined"
    
    in let emit_column x = match x with
       | {col_source = COLUMN(q, c); col_alias = a} -> Printf.sprintf "%s.%s as %s" q c (alias a)
    
    in let emit_select ?(ordby = None) cols from    =
        let q = Printf.sprintf "select \n%s\nfrom %s" cols from
        in match ordby with Some(c) -> q ^ (Printf.sprintf "\norder by\n%s" c)
                            | _     -> q

    in let emit_select_cols  columns = 
        String.join ",\n" (List.map (fun x -> ident idnt x)
                                    (List.map emit_column columns))

    in let wrap_subquery sq name = Printf.sprintf "(%s) as %s" sq name 

    in let wrap_subquery_cols (cols:col_t list) name = 
        List.map (fun x -> { x with col_source = COLUMN(name, (alias x.col_alias)) } ) cols

    in let emit_ordby cols =
        String.join ",\n" (List.map (fun {col_alias=a; col_order=o} -> (i1 (Printf.sprintf "%s%s" (alias a) "")) ) 
                                    cols)  

    in let rep  = normalize_report x


    in let cols  = emit_select_cols rep.columns

    in let ord_cols = List.filter (function  {col_order = Some(x)} -> true | _ -> false) rep.columns
   
    in let nested = List.length ord_cols > 0

    in let sel = emit_select cols ""

    in if not nested 
       then sel 
       else emit_select (emit_select_cols (wrap_subquery_cols rep.columns "sq")) 
                        (wrap_subquery sel "sq") 
                        ~ordby:(Some(emit_ordby ord_cols))

