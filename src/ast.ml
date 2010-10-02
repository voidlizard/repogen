open ExtList
open ExtString

type report = Report of report_t
and columns  = column list
and fields   = field list
and field    = Field of field_t
and column   = Column of column_t
and report_t = { fields: field list; columns: column list }
and field_t  = { field_id  : int; field_name : string }
and column_t = { column_alias: string option;
                 column_name : string option;
                 column_sort: sort_type option;
                 column_fold: bool }
and sort_type = ASC | DESC

type col_attrib = | SORT of sort_type
                  | ALIAS of string
                  | NAME of string
                  | FOLD of bool


type item = C of column | F of field


let report e =
  Report( List.fold_left (fun rt a -> match a with
            | C(c)  -> { rt with columns = c :: rt.columns }
            | F(f)  -> { rt with fields  = f :: rt.fields }
             ) { fields = []; columns = [] } e )

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

let c col = C(col)
let f fld = F(fld)

let dump (Report({columns=cols; fields=fields})) =
  let ident = "    "
  in let quote s = "'" ^ s ^ "'"
  in let dump_column i c = Printf.sprintf "%s %-8s %-16s\n" i (Option.default  "<undef>" c.column_alias)
                                                               (quote (Option.default  "<undef>" c.column_name))
  in let dump_field  i f = assert false
  in Printf.sprintf "FIELDS\n%s\nCOLUMNS\n%s"
                    (String.join "" (List.map ( fun (Field(f))  -> dump_field  ident f) fields ))
                    (String.join "" (List.map ( fun (Column(c)) -> dump_column ident c ) cols  ))

