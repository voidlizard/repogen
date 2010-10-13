%{

module B = Report_builder
module R = Report

%}

%token OBR CBR
%token <int> INT
%token <string> STRING
%token <string> NUMBER
%token <string> IDENT
%token DOT
%token FIELD COLUMN END ALIAS NAME SOURCE FILTER SORT FOLD
%token GROUP
%token NONE YES NO NONE ASC DESC
%token CONNECTION DATASOURCE TABLE
%token TEMPLATE TEMPLATE_DIRS
%token OUTPUT FILE TEMPORARY STDOUT
%token POSTPROCESS ECHO ABORT
%token BEFORE AFTER
%token EQ NE LT GT LE GE 
%token OR AND IN BETWEEN  
%token LIKE

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
    | postprocess           { $1 }
    | misc_actions          { $1 }

column:
    | COLUMN column_attribs END  { $2 }

column_attribs:                     { [] }
    | column_attrib column_attribs  { $1 :: $2 }

column_attrib:
    | ALIAS  IDENT               { B.with_col_alias $2 }
    | NAME   STRING              { B.with_col_name $2  }
    | SOURCE col_ref             { B.with_col_source $2 }
    | filter                     { $1 }
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

postprocess:
    | POSTPROCESS STRING             { B.with_postprocess $2 }


misc_actions:
    | ECHO BEFORE STRING             { B.with_echo R.BEFORE $3 }
    | ECHO AFTER  STRING             { B.with_echo R.AFTER  $3 }
    | ECHO STRING                    { B.with_echo R.BEFORE $2 }
    | ABORT                          { B.with_abort R.BEFORE }
    | ABORT BEFORE                   { B.with_abort R.BEFORE }
    | ABORT AFTER                    { B.with_abort R.AFTER }

filter:
    | FILTER filter_eq               { $2 }

filter_eq:
    | EQ filt_single_arg             { assert false }
    | NE filt_single_arg             { assert false }
    | LT filt_single_arg             { assert false }
    | GT filt_single_arg             { assert false }
    | LE filt_single_arg             { assert false }
    | GE filt_single_arg             { assert false }
    | LIKE filt_like_arg             { B.with_col_filt (R.LIKE($2)) }

filt_single_arg:
    | OBR filt_arg CBR               { assert false }


filt_arg:
    | NUMBER                         { assert false }
    | STRING                         { assert false }

filt_like_arg:
    | OBR STRING CBR                 { $2 }

%%
