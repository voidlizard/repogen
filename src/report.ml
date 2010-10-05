open ExtList
open ExtString


type report_t = { columns: col_t list }
and col_t = { col_name: string option;
              col_alias: string option;
              col_order: order_t option}
and order_t = ASC | DESC


let ident i s = Printf.sprintf "%s %s" i s


let rec normalize_report rep =
    { rep with columns = normalize_columns rep.columns }
and normalize_columns cols = List.mapi normalize_column cols
and normalize_column i  = function ({ col_alias = None } as c)  -> { c with col_alias = Some(Printf.sprintf "col%d" i) }
                                  |({ col_alias = Some _} as c) -> c


let sql_of x =
    let idnt = "   "
    in let emit_as s = Printf.sprintf "'' as %s" s
    in let emit_select cols from = Printf.sprintf "select \n%s\nfrom %s" cols from
    in let wrap_subquery sq = Printf.sprintf "(%s) as sq" sq


    in let alias x = try Option.get x with Option.No_value -> failwith "Alias is not defined"
    in let rep  = normalize_report x

    in let cols = String.join ",\n" (List.map (fun x -> ident idnt x)
                                              (List.map (fun x -> emit_as (alias x.col_alias)) rep.columns))

    in let ord_cols = List.filter (function  {col_order = Some(x)} -> true | _ -> false) rep.columns
   
    in let nested = List.length ord_cols > 0

    in if not nested 
       then emit_select cols ""
       else emit_select cols (wrap_subquery (emit_select cols ""))


