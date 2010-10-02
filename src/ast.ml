open ExtList
open ExtString

type report = Report of report_t
and columns  = column list
and fields   = field list
and field    = Field of field_t
and column   = Column of column_t
and connection = string * string
and datasource = DATASOURCE of datasource_t
and datasource_type_t = DS_TABLE
and sort_type = ASC | DESC
and col_src_t = SRC_REF of col_ref
and report_t = { fields: field list;
                 columns: column list;
                 connection: connection option;
                 datasources: (string * datasource) list }
and field_t  = { field_id  : int; field_name : string  }
and column_t = { column_alias: string option;
                 column_name : string option;
                 column_sort: sort_type option;
                 column_source: col_src_t option;
                 column_fold: bool }
and datasource_t = { ds_alias: string; ds_source: string; ds_type: datasource_type_t }
and col_ref = { colref_src: string; colref_col: string }

type col_attrib = | SORT of sort_type
                  | ALIAS of string
                  | NAME of string
                  | FOLD of bool
                  | SOURCE of col_ref

type ds_attrib = DS_SOURCE of string | DS_ALIAS of string


type item = C of column | F of field | CO of connection | DS of datasource

let report e =
  Report( List.fold_left (fun rt a -> match a with
            | C(c)   -> { rt with columns = c :: rt.columns }
            | F(f)   -> { rt with fields  = f :: rt.fields }
            | CO(cn) -> { rt with connection = Some(cn) }
            | DS(DATASOURCE(d))  -> { rt with datasources = (d.ds_alias, DATASOURCE(d)) :: rt.datasources }
             ) { fields = []; columns = []; connection = None; datasources = [] } e )

let column attr =
  Column( List.fold_left (fun ct a -> match a with
            | SORT(ASC)   -> { ct with column_sort  = Some(ASC)  }
            | SORT(DESC)  -> { ct with column_sort  = Some(DESC) }
            | FOLD(true)  -> { ct with column_fold  = true }
            | FOLD(false) -> { ct with column_fold  = false }
            | ALIAS(s)    -> { ct with column_alias = Some(s) }
            | SOURCE(s)   -> { ct with column_source = Some(SRC_REF(s)) }
            | NAME(s)     -> { ct with column_name  = Some(s) } )
                          { column_alias = None;
                            column_name  = None;
                            column_sort  = None;
                            column_source = None;
                            column_fold  = false } attr)

let col_source s = SOURCE(s)

let col_ref s c = { colref_src = s; colref_col = c }

let sort s = SORT(s)

let alias s = ALIAS(s)

let name s = NAME(s)

let fold t = FOLD(t)

let fold_yes () = true

let fold_no () = false

let conn x = CO(x)

let ds x = DS(x)

let connection id s = (id, s)

let ds_source s = DS_SOURCE(s)

let ds_alias  s = DS_ALIAS(s)

let datasource_table ds =
  DATASOURCE( List.fold_left (fun dt a -> match a with
    | DS_ALIAS(s)  -> { dt with ds_alias = s }
    | DS_SOURCE(s) -> { dt with ds_source = s}) {ds_alias = ""; ds_source = ""; ds_type = DS_TABLE } ds)

let c col = C(col)
let f fld = F(fld)

let dump (Report({columns=cols; fields=fields; connection=conn; datasources=ds})) =
  let ident = "    "

  in let quote s = "'" ^ s ^ "'"

  in let dump_ds_type = function DS_TABLE -> "table"

  in let dump_ds i (n,DATASOURCE({ds_source=s;ds_type=ds_t})) = Printf.sprintf "%s %-8s %-8s %s" i n
                                                                                                 (dump_ds_type ds_t)
                                                                                                 (quote s)
  in let dump_connect i = function
    | Some((id,s)) -> Printf.sprintf "%s %-8s %s" i id (quote s)
    | None         -> Printf.sprintf "n/a"


  in let dump_col_src = function
    | Some(SRC_REF{colref_src=s;colref_col=c}) -> Printf.sprintf "%s.%s" s c
    | None                                     -> "<undef>"

  in let dump_column  i c  = Printf.sprintf "%s %-8s %-16s fold:%-5s sort:%-5s %-16s"
                                             i (Option.default  "<undef>" c.column_alias)
                                               ( dump_col_src c.column_source )
                                               ( if c.column_fold then "true" else "false")
                                               ( match c.column_sort with
                                                 | Some(ASC)  -> "asc"
                                                 | Some(DESC) -> "desc"
                                                 | None       -> "none")
                                               (quote (Option.default  "<undef>" c.column_name))
  in let dump_field  i f = assert false

  in Printf.sprintf "CONNECTIONS\n%s\n\nDATASOURCES\n%s\n\nFIELDS\n%s\nCOLUMNS\n%s\n"
                    (String.join "\n" (List.map ( fun c           -> dump_connect ident c) (conn :: []) ))
                    (String.join "\n" (List.map ( fun d           -> dump_ds ident d) ds   ))
                    (String.join "\n" (List.map ( fun (Field(f))  -> dump_field   ident f) fields ))
                    (String.join "\n" (List.map ( fun (Column(c)) -> dump_column  ident c ) cols  ))

