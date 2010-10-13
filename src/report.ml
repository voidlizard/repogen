open ExtList
open ExtString
open Util

module P  = Printf
module L  = List
module DB = Db_pg

type report_t = { columns: col_t list;
                  datasources: (string * datasource_t) list;
                  connections: (string * connection_t) list;
                  template: string option;
                  template_dirs: string list;
                  output: output_t;
                  pre_actions:  action_t list;
                  post_actions: action_t list;
                  vars: (string * varfun_t) list;
                  query_args: (string * val_t) list
                }
and col_t = { col_name: string option;
              col_alias: string option;
              col_order: order_t option;
              col_source: source_t;
              col_group: bool;
              col_filter: filt_op_t option
            }
and order_t = ASC | DESC
and source_t = COLUMN of string * string
and datasource_t = DS_TABLE of string
and connection_t = string
and output_t = STDOUT | FILE of string
and action_when_t = BEFORE | AFTER
and action_t = ( report_t -> report_t )
and varfun_t = ( report_t -> string )
and filt_op_t = LIKE of val_t
and val_t = STR_CONST of string | VAR_REF of string

let ident i s = P.sprintf "%s %s" i s

let ident_all i s = List.map (fun x -> ident i x) s


let enum_file_names dirs fname = 
    List.map ( fun x -> Filename.concat x fname ) dirs

let filt_by_permiss perm fnames = 
    List.filter ( fun f -> try (Unix.access f perm ; true)
                           with Unix.Unix_error _ -> false ) fnames

let rec normalize_report rep =
    { rep with columns  = normalize_columns rep.columns;
               template = normalize_template rep.template_dirs rep.template }

and normalize_columns cols = List.mapi normalize_column cols
and normalize_column i  = function ({ col_alias = None } as c)  -> { c with col_alias = Some(P.sprintf "col%d" i) }
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

let execute_actions t report = 
    match t with 
    | BEFORE -> List.fold_left (fun acc x -> x acc) report report.pre_actions
    | AFTER  -> List.fold_left (fun acc x -> x acc) report report.post_actions


let str_of_val = function
    | STR_CONST(s) -> s
    | VAR_REF(s)   -> P.sprintf "${%s}" s

let sql_of rep  =
    let idnt = "   "
    in let i1 = ident idnt
    in let alias x = try Option.get x with Option.No_value -> failwith "Alias is not defined"
   
    in let fcn = function COLUMN(table, name) -> P.sprintf "%s.%s" table name

    in let emit_column x = match x with
       | {col_source = cs; col_alias = a} -> P.sprintf "%s as %s" (fcn cs) (alias a)
   

    in let rec emit_select ?groupby:(g = None)
                           ?ordby:(o = None) 
                           ?where:(w = None) 
                           cols f =
        select cols
               (from f)
               (where w)
               (group_by g)
               (order_by o)

    and from f = P.sprintf "from %s" f
    and group_by = function Some(g) -> Some(P.sprintf "group by\n%s" g) | None -> None
    and order_by = function Some(g) -> Some(P.sprintf "order by\n%s" g) | None -> None
    and where    = function Some(g) -> Some(P.sprintf "where\n%s" g)    | None -> None
    and select c f w g o = List.fold_left (fun q p -> match p with Some(s) -> q @ [s] | _ -> q)
                           ["select"; c ; f] [w;g;o]
                           |> String.join "\n"

    in let ds_of_col rep = function {col_source = COLUMN(n, c)} ->
        try (n, List.assoc n rep.datasources)
        with Not_found -> failwith (P.sprintf "No datasource definition: %s" n)

    in let emit_select_cols  columns = 
        String.join ",\n" (List.map (fun x -> ident idnt x)
                                    (List.map emit_column columns))

    in let wrap_subquery sq name = P.sprintf "(%s) as %s" sq name 

    in let wrap_subquery_cols (cols:col_t list) name = 
        List.map (fun x -> { x with col_source = COLUMN(name, (alias x.col_alias)) } ) cols

    in let emit_ord_cnd  = function Some(ASC)  -> " asc" 
                                  | Some(DESC) -> " desc"
                                  | _          -> "" 

    in let emit_ordby cols =
        String.join ",\n" 
                    (List.map (fun {col_source=cs; col_order=o} -> (i1 (P.sprintf "%s%s" (fcn cs) (emit_ord_cnd o))) ) 
                              cols)

    in let emit_groupby cols = 
        String.join ",\n" 
                    (List.map (fun {col_source=cs} -> (i1 (P.sprintf "%s" (fcn cs) )) ) 
                              cols)

    in let emit_from rep = 
        let ds = List.map (fun x -> ds_of_col rep x) rep.columns |> List.unique
        in let _ = if List.length ds > 1 then failwith "Several datasources found. Joins are not supported yet"
        in let (n, DS_TABLE(s)) = List.hd ds
        in P.sprintf "%s %s" s n


    in let emit_where ?idnt:(i="") rep =
        let flts = List.fold_left ( fun acc c -> match c.col_filter with 
                                               | None -> acc 
                                               | Some(x) -> (x, c.col_source) :: acc
                                  ) [] rep.columns
        in let rec emit cnd = function
            | (LIKE(VAR_REF(s)), c)   :: xs -> emit ((P.sprintf "%s like ${%s}" (fcn c) s)::cnd) xs
            | (LIKE(STR_CONST(s)), c) :: xs -> emit ((P.sprintf "%s like '%s'" (fcn c) s)::cnd) xs
            | []                 -> cnd
        in let cnd = String.join "\nand " (emit [] flts) 
        in (P.sprintf "%s" cnd)


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

    in let flt = emit_where rep

    in let filter = if String.length flt > 0
                    then Some(flt)
                    else None

    in let sel = emit_select cols (emit_from rep) ~where:filter

    in if not nested 
       then sel 
       else emit_select (emit_select_cols (wrap_subquery_cols rep.columns sq)) 
                        (wrap_subquery sel sq) 
                        ~ordby:(Some(emit_ordby ord_cols))
                        ~groupby:groupby


let parametrized_sql report =
    let sql = sql_of report
    in let repl = List.mapi (fun i (n,v) -> (n, (DB.placeholder (i+1) n))) report.query_args
    in let vals = List.map (fun (n,x) -> str_of_val x) report.query_args
    in (Stmpl.parse_string sql repl, vals)

let report_col_name col = 
    match col.col_alias with 
    | Some(x) -> x
    | None -> ( match col.col_source with COLUMN(a,s) -> P.sprintf "%s_%s" a s )  

let metavars report =
    List.map ( fun (n,f) -> (n, f report) ) report.vars

let extract_query_args col =

    let argn = P.sprintf "QUERY_ARG_%s" (String.uppercase (report_col_name col))

    in let rec extract col = match col.col_filter with
        | Some(LIKE(STR_CONST(s) as c)) as f  -> ([(argn, STR_CONST(s))],
                                                  {col with col_filter = Some(LIKE(VAR_REF(argn)))})
        | None                                -> ([], col)
        | x                                   -> assert false
    in extract col



