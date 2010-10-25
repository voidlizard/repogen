open ExtList
open ExtString
open Util

module P  = Printf
module L  = List
module DB = Db_pg

type report_t = { columns: col_t list;
                  fields: field_t list;
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
              col_filter: filt_op_t option;
              col_fold: bool
            }
and order_t = ORDER of order_type_t * nulls_t option
and order_type_t = ASC | DESC
and nulls_t = NULLS_FIRST | NULLS_LAST
and source_t = COLUMN of string * string
and datasource_t = DS_TABLE of string
and connection_t = string
and output_t = STDOUT | FILE of string
and action_when_t = BEFORE | AFTER
and action_t = ( report_t -> report_t )
and varfun_t = ( report_t -> string )
and filt_op_t = LIKE of val_t | EQ of val_t | NE of val_t
                | LT of val_t | GT of val_t | LE of val_t 
                | GE of val_t
                | BETWEEN of (val_t * val_t)
                | OR of filt_op_t * filt_op_t
                | AND of filt_op_t * filt_op_t
                | NOT of filt_op_t
and fun_ns_t = SQL
and fun_arg_t = FA_ALIAS of string
and fun_call_t = { fun_ns: fun_ns_t; fun_name: string; fun_args: fun_arg_t list }
and field_t = { field_alias: string; field_source: field_src_t; field_flt: (filt_op_t * string) list}
and field_src_t = FIELD_FUN_CALL of fun_call_t

and val_t = STR_CONST of string | NUM_CONST of string | VAR_REF of string

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
    | NUM_CONST(s) -> s
    | STR_CONST(s) -> s
    | VAR_REF(s)   -> P.sprintf "${%s}" s


let fcn = function COLUMN(table, name) -> P.sprintf "%s.%s" table name

let rec emit_flt flt = List.map (fun (f, src) -> op src f) flt
and op src v = match v with 
| NOT(a)   -> P.sprintf "not (%s)" (op src a) 
| OR(a,b)  -> P.sprintf "(%s or %s)" (op src a) (op src b)
| AND(a,b) -> P.sprintf "(%s and %s)" (op src a) (op src b)
| BETWEEN(a, b) -> P.sprintf "%s between %s and %s" (fcn src) (quote a) (quote b)
| _      -> P.sprintf "%s %s %s" (fcn src) (op_of v) (quote (val_of v))
and op_of = function
    | LIKE _ -> "like"
    | GT _   -> ">"
    | LT _   -> "<"
    | GE _   -> ">="
    | LE _   -> "<="
    | NE _   -> "!="
    | EQ _   -> "="
    | _      -> assert false
and quote = function
    | STR_CONST(s) -> P.sprintf "'%s'" s
    | NUM_CONST(v) -> v
    | VAR_REF(n) -> P.sprintf "${%s}" n
and val_of = function
    | LIKE(v) | GT(v) | LT(v) | GE(v) 
    | LE(v) | LE(v) | NE(v) | EQ(v) -> v
    | _ -> assert false


let has_sql_fields rep = 
    List.length (List.filter 
                        (function {field_source=FIELD_FUN_CALL({fun_ns=SQL})} -> true
                                  | _ -> false)
                        rep.fields) > 0

let rec emit_sql_fun r fn args = 
    let argz = List.map (fun a -> emit_sql_fun_arg r a) args |> String.join ","
    in P.sprintf "%s(%s)" fn argz
and emit_sql_fun_arg r = function
    | FA_ALIAS(s) -> s

let sql_of_field rep f tbl =
    let filt = List.map ( fun (f,col) -> (f, COLUMN(tbl, col)) ) 
    in let where_of x = if x != [] 
                        then P.sprintf "where %s" (String.join " and " (emit_flt x))
                        else ""
    in let src = match f.field_source with
        | FIELD_FUN_CALL({fun_ns=SQL; fun_name=n; fun_args=a})
            -> P.sprintf "(select %s from %s %s)" (emit_sql_fun rep n a) tbl 
                                                  (where_of (filt f.field_flt))
        | _ -> failwith "Unsupported field type"
    in src

let fields_of report = List.map (function {field_alias=a} -> a) report.fields

let sql_of_fields rep tbl = 
    let cols = List.map (fun x -> sql_of_field rep x tbl) rep.fields |> String.join ",\n"
    in P.sprintf "select %s \nlimit 1" cols

let sql_of rep  =
    let idnt = "   "
    in let i1 = ident idnt
    in let alias x = try Option.get x with Option.No_value -> failwith "Alias is not defined"
   
    in let emit_column x = match x with
       | {col_source = cs; col_alias = a; } -> P.sprintf "%s as %s" (fcn cs) (alias a)

    in let emit_column_allow_fold x = match x with
       | {col_source = cs; col_alias = a; col_fold = true}  -> let fc = fcn cs
           in P.sprintf "(case when lag(%s) over () is distinct from %s then %s else null end) as %s" fc fc fc (alias a)
       | _ -> emit_column x

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

    in let emit_select_cols ?col_emitter:(e = emit_column) columns = 
        String.join ",\n" (List.map (fun x -> ident idnt x) (List.map e columns))

    in let wrap_subquery sq name = P.sprintf "(%s) as %s" sq name 

    in let wrap_subquery_cols (cols:col_t list) name = 
        List.map (fun x -> { x with col_source = COLUMN(name, (alias x.col_alias)) } ) cols

    in let rec emit_ord_cnd  = function Some(ORDER(ASC, v))  -> " asc" ^ (emit_nulls v)
                                      | Some(ORDER(DESC, v)) -> " desc" ^ (emit_nulls v)
                                      | _                    -> ""
       and emit_nulls = function   Some(NULLS_FIRST) -> " nulls first"
                                 | Some(NULLS_LAST)  -> " nulls last"
                                 | None              -> ""

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

        in let cnd = String.join "\nand " (emit_flt flts) 
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

    in let sql =
       if not nested 
       then sel 
       else emit_select (emit_select_cols (wrap_subquery_cols rep.columns sq))
                        (wrap_subquery sel sq) 
                        ~ordby:(Some(emit_ordby ord_cols))
                        ~groupby:groupby


    in let folding = List.fold_left (fun acc c -> acc || c.col_fold) false ord_cols

    in let sql = if folding
                 then emit_select (emit_select_cols (wrap_subquery_cols rep.columns "sq2")
                                                    ~col_emitter:emit_column_allow_fold)
                                  (wrap_subquery sql "sq2")
                 else sql

    in sql 


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

    let argn (l,s) = P.sprintf "QUERY_ARG_%s%s" (String.uppercase (report_col_name col))
                                                (if s <> "" then P.sprintf "_%s%d" s l
                                                            else "" )

    in let rec extract col = 

        let rec extr ((l,s) as p) v = 
            let arg = argn p
            in match v with
                | LIKE(VAR_REF(_))
                | EQ(VAR_REF(_))
                | NE(VAR_REF(_))
                | LT(VAR_REF(_))
                | GT(VAR_REF(_))
                | GE(VAR_REF(_))
                | LE(VAR_REF(_))  -> ([], v)

                | LIKE(x)  -> ([(arg, x)], LIKE(VAR_REF(arg))) 
                | EQ(x)    -> ([(arg, x)], EQ(VAR_REF(arg)))
                | NE(x)    -> ([(arg, x)], NE(VAR_REF(arg)))
                | LT(x)    -> ([(arg, x)], LT(VAR_REF(arg)))
                | GT(x)    -> ([(arg, x)], GT(VAR_REF(arg))) 
                | GE(x)    -> ([(arg, x)], GE(VAR_REF(arg)))
                | LE(x)    -> ([(arg, x)], LE(VAR_REF(arg)))

                | BETWEEN(a,b) -> let vs = extr_vals p [a;b]
                                  in let args = List.map2 ( fun z1 z2 -> match (z1, z2) with
                                                         | (Some((n, v)), zz) -> VAR_REF(n) 
                                                         | (None, zz) -> zz
                                                       )
                                                       vs [a;b]
                                  in let argz = List.filter Option.is_some vs |> List.map Option.get
                                  in let (x1,x2) = match args with
                                                   | x :: y :: [] -> (x,y)
                                                   | _            -> assert false
                                  in (argz, BETWEEN(x1, x2))

                | NOT(a)   -> let (args, x) = extr p a in (args, NOT(x)) 
                | AND(a,b) -> let (args, l, r) = extr_bin l a b in (args, AND(l, r))
                | OR(a,b)  -> let (args, l, r) = extr_bin l a b in (args, OR(l, r))
                          
        and extr_bin l a b = 
            let (args1, x1) = extr ((l+1),"L") a
            in let (args2, x2) = extr ((l+1),"R") b
            in (args1 @ args2, x1, x2) 

        and extr_vals (l,s) vs =
            List.mapi ( fun i x -> match x with
                                   | VAR_REF _ -> None
                                   | x         -> Some((argn (l, (P.sprintf "P%d_%s" i s)), x))
                      ) vs
                        

        in match col.col_filter with
            | Some(x) -> let (args, flt) = extr (0,"") x in (args, {col with col_filter = Some(flt)}) 
            | None    -> ([], col)
    
    in extract col



