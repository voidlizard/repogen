open ExtList
open ExtString

type report = Report of report_t
and columns  = column list
and fields   = field list
and field    = Field of field_t
and column   = Column of column_t
and report_t = { fields: field list; columns: column list; connections: connection list }
and field_t  = { field_id  : int; field_name : string }
and column_t = { column_alias: string option;
                 column_name : string option;
                 column_sort: sort_type option;
                 column_fold: bool }
and connection = string * string
and sort_type = ASC | DESC

type col_attrib = | SORT of sort_type
                  | ALIAS of string
                  | NAME of string
                  | FOLD of bool


type item = C of column | F of field | CO of connection


let report e =
  Report( List.fold_left (fun rt a -> match a with
            | C(c)   -> { rt with columns = c :: rt.columns }
            | F(f)   -> { rt with fields  = f :: rt.fields }
            | CO(cn) -> { rt with connections = cn :: rt.connections }
             ) { fields = []; columns = []; connections = [] } e )

let column attr =
  Column( List.fold_left (fun ct a -> match a with
            | SORT(ASC)   -> { ct with column_sort  = Some(ASC)  }
            | SORT(DESC)  -> { ct with column_sort  = Some(DESC) }
            | FOLD(true)  -> { ct with column_fold  = true }
            | FOLD(false) -> { ct with column_fold  = false }
            | ALIAS(s)    -> { ct with column_alias = Some(s) }
            | NAME(s)     -> { ct with column_name  = Some(s) } )
                          { column_alias = None;
                            column_name  = None;
                            column_sort  = None;
                            column_fold  = false } attr)

let sort s = SORT(s)

let alias s = ALIAS(s)

let name s = NAME(s)

let fold t = FOLD(t)

let fold_yes () = true

let fold_no () = false

let conn x = CO(x)

let connection id s = (id, s)

let c col = C(col)
let f fld = F(fld)

let dump (Report({columns=cols; fields=fields; connections=conn})) =
  let ident = "    "
  in let quote s = "'" ^ s ^ "'"
  in let dump_connect i (id,s) = Printf.sprintf "%s %-8s %s\n" i id s
  in let dump_column  i c  = Printf.sprintf "%s %-8s %-16s\n" i (Option.default  "<undef>" c.column_alias)
                                                              (quote (Option.default  "<undef>" c.column_name))
  in let dump_field  i f = assert false
  in Printf.sprintf "CONNECTIONS\n%s\nFIELDS\n%s\nCOLUMNS\n%s"
                    (String.join "" (List.map ( fun c           -> dump_connect ident c) conn   ))
                    (String.join "" (List.map ( fun (Field(f))  -> dump_field   ident f) fields ))
                    (String.join "" (List.map ( fun (Column(c)) -> dump_column  ident c ) cols  ))

