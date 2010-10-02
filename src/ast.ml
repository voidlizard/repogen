type ast = Report of fields * column
and columns  = column list
and fields   = field list
and field    = Field of field_t
and column   = Column of column_t
and field_t  = { field_id  : int; field_name : string }
and column_t = { column_id : int; column_name : string; column_sort: sort_type option; column_fold: bool }
and sort_type = ASC | DESC

type col_attrib = | SORT of sort_type option
                  | ALIAS of string
                  | NAME of string
                  | FOLD of bool


let column attr = assert false

let sort s = SORT(s)

let alias s = ALIAS(s)

let name s = NAME(s)

let fold t = FOLD(t)

let fold_yes () = true

let fold_no () = false

