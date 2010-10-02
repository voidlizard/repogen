type ast = Report of fields * column
and columns = column list
and fields  = field list
and field   = { field_id  : int; field_name : string }
and column  = { column_id : int; column_name : string }
