%{

module B = Report_builder

%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token DOT
%token FIELD COLUMN END ALIAS NAME SOURCE FILTER SORT FOLD
%token GROUP
%token NONE YES NO NONE ASC DESC
%token CONNECTION DATASOURCE TABLE
%token TEMPLATE TEMPLATE_DIRS
%token OUTPUT FILE TEMPORARY STDOUT
%token EOF

%start toplevel
%type <Report.report_t> toplevel

%%

toplevel:
    | entries               { B.build_report $1 }
    | EOF                   { assert false  }

entries:                    { [] }
    | entry entries         { $1 :: $2 }

entry:
    | field                 { assert false }
    | template              { $1 }
    | template_dirs         { $1 }
    | column                { B.with_column $1 }
    | connection            { B.with_connection $1 }
    | datasource            { B.with_datasource $1 }
    | output                { $1 }

column:
    | COLUMN column_attribs END  { $2 }

column_attribs:                     { [] }
    | column_attrib column_attribs  { $1 :: $2 }

column_attrib:
    | ALIAS  IDENT               { B.with_col_alias $2 }
    | NAME   STRING              { B.with_col_name $2  }
    | SOURCE col_ref             { B.with_col_source $2 }
    | FILTER IDENT               { failwith "FILTER is not supported yet" }
    | SORT   sort_args           { B.with_col_order $2 }
    | GROUP                      { B.with_group }
    | FOLD fold_arg              { assert false }

sort_args:
    | ASC                        { B.col_order_asc  () }
    | DESC                       { B.col_order_desc () }

fold_arg:
    | YES                        { failwith "FOLD is not supported yet" }
    | NO                         { failwith "FOLD is not supported yet" }

col_ref:
    | IDENT DOT IDENT            { B.col_ref $1 $3 }

field:
    | FIELD END                  { failwith "FIELD is not supported yet" }

template:
    | TEMPLATE STRING            { B.with_template $2 }

template_dirs:
    | TEMPLATE_DIRS STRING       { B.with_template_dirs $2 }

connection:
    | CONNECTION IDENT STRING    { ( $2, $3) }

datasource:
    | DATASOURCE TABLE datasource_args END  { B.with_datasource_table $3 }

datasource_args:                 { [] }
    | datasource_arg datasource_args { $1 :: $2 }

datasource_arg:
    | ALIAS  IDENT                { B.datasource_alias $2  }
    | SOURCE IDENT                { B.datasource_source $2 }

output:
    | OUTPUT TEMPORARY               { B.with_output_temp () }
    | OUTPUT TEMPORARY STRING STRING { B.with_output_temp ~prefix:$3 ~suffix:$4 () }
    | OUTPUT STDOUT                  { B.with_output_stdout () }
    | OUTPUT FILE STRING             { B.with_output_file $3 }

%%
