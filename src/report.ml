open ExtList
open ExtString
open Util

type report_t = { columns: col_t list;
                  datasources: (string * datasource_t) list;
                  connections: (string * connection_t) list;
                  template: string option;
                  template_dirs: string list
                }
and col_t = { col_name: string option;
              col_alias: string option;
              col_order: order_t option;
              col_source: source_t;
              col_group: bool 
            }
and order_t = ASC | DESC
and source_t = COLUMN of string * string
and datasource_t = DS_TABLE of string
and connection_t = string

let ident i s = Printf.sprintf "%s %s" i s

let ident_all i s = List.map (fun x -> ident i x) s


let enum_file_names dirs fname = 
    List.map ( fun x -> Filename.concat x fname ) dirs

let filt_by_permiss perm fnames = 
    List.filter ( fun f -> try (Unix.access f perm ; true)
                           with Unix.Unix_error _ -> false ) fnames

let rec normalize_report rep =
    { rep with columns = normalize_columns rep.columns;
               template = normalize_template rep.template_dirs rep.template }
and normalize_columns cols = List.mapi normalize_column cols
and normalize_column i  = function ({ col_alias = None } as c)  -> { c with col_alias = Some(Printf.sprintf "col%d" i) }
                                  |({ col_alias = Some _} as c) -> c

and normalize_template dirs fname =
    match fname with 
    | Some(x) -> (let fnames = enum_file_names dirs x |> filt_by_permiss [Unix.F_OK; Unix.R_OK]
                 in match fnames with 
                    | x :: _ -> Some(x)
                    | []     -> None)
    
    | None    -> None

let column_headers report = 
    List.map ( function   { col_name = Some(n); col_alias = Some(a) } -> (a, n)
                        | { col_name = None;    col_alias = Some(a) } -> (a, a)
                        | { col_name = None;    col_alias = None }    -> ("undef", "undef")
                        | { col_name = Some(x); col_alias = None }    -> assert false )
             report.columns

let connection_of r = snd (List.hd r.connections)

let sql_of rep =
    let idnt = "   "
    in let i1 = ident idnt
    in let alias x = try Option.get x with Option.No_value -> failwith "Alias is not defined"
   
    in let fcn = function COLUMN(table, name) -> Printf.sprintf "%s.%s" table name

    in let emit_column x = match x with
       | {col_source = cs; col_alias = a} -> Printf.sprintf "%s as %s" (fcn cs) (alias a)
    
    in let emit_select ?(groupby = None) ?(ordby = None) cols from    =
        let q = Printf.sprintf "select \n%s\nfrom %s" cols from
        in let q2 = match groupby with Some(c) -> q ^ (Printf.sprintf "\ngroup by\n%s" c)
                                      | _      -> q
        in let q3 = match ordby with Some(c) -> q2 ^ (Printf.sprintf "\norder by\n%s" c)
                                    | _      -> q2
        in q3

    in let ds_of_col rep = function {col_source = COLUMN(n, c)} ->
        try (n, List.assoc n rep.datasources)
        with Not_found -> failwith (Printf.sprintf "No datasource definition: %s" n)

    in let emit_select_cols  columns = 
        String.join ",\n" (List.map (fun x -> ident idnt x)
                                    (List.map emit_column columns))

    in let wrap_subquery sq name = Printf.sprintf "(%s) as %s" sq name 

    in let wrap_subquery_cols (cols:col_t list) name = 
        List.map (fun x -> { x with col_source = COLUMN(name, (alias x.col_alias)) } ) cols

    in let emit_ord_cnd  = function Some(ASC)  -> " asc" 
                                  | Some(DESC) -> " desc"
                                  | _          -> "" 

    in let emit_ordby cols =
        String.join ",\n" 
                    (List.map (fun {col_source=cs; col_order=o} -> (i1 (Printf.sprintf "%s%s" (fcn cs) (emit_ord_cnd o))) ) 
                              cols)

    in let emit_groupby cols = 
        String.join ",\n" 
                    (List.map (fun {col_source=cs} -> (i1 (Printf.sprintf "%s" (fcn cs) )) ) 
                              cols)

    in let emit_from rep = 
        let ds = List.map (fun x -> ds_of_col rep x) rep.columns |> List.unique
        in let _ = if List.length ds > 1 then failwith "Several datasources found. Joins are not supported yet"
        in let (n, DS_TABLE(s)) = List.hd ds
        in Printf.sprintf "%s %s" s n

    in let cols  = emit_select_cols rep.columns

    in let sq = "sq"

    in let ord_cols = (wrap_subquery_cols (List.filter (function  {col_order = Some(x)} -> true | _ -> false)
                                          rep.columns)
                                          sq)
 
    in let group_cols = (wrap_subquery_cols (List.filter (fun x  -> x.col_group) rep.columns) sq)

    in let groupby = if List.length group_cols > 0
                     then Some(emit_groupby group_cols)
                     else None

    in let nested = List.length ord_cols > 0 || List.length group_cols > 0

    in let sel = emit_select cols (emit_from rep)

    in if not nested 
       then sel 
       else emit_select (emit_select_cols (wrap_subquery_cols rep.columns sq)) 
                        (wrap_subquery sel sq) 
                        ~ordby:(Some(emit_ordby ord_cols))
                        ~groupby:groupby


