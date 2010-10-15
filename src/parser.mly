%{

module B = Report_builder
module R = Report
module P = Printf

%}

%token OBR CBR
%token <int> INT
%token <string> STRING
%token <string> NUMBER
%token <string> IDENT
%token DOT COMMA
%token FIELD COLUMN END ALIAS NAME SOURCE FILTER SORT FOLD
%token GROUP
%token NONE YES NO NONE ASC DESC
%token CONNECTION DATASOURCE TABLE
%token TEMPLATE TEMPLATE_DIRS
%token OUTPUT FILE TEMPORARY STDOUT
%token POSTPROCESS ECHO ABORT
%token BEFORE AFTER
%token EQ NE LT GT LE GE 
%token OR AND NOT
%token IN BETWEEN  
%token SQL
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
    | FIELD field_decl END       { failwith "FIELD is not supported yet" }

field_decl:
    | field_entry field_decl     { failwith "FIELD ENTRY" }

field_entry:
    NAME STRING                  { failwith "JOPA!" }
//    | NAME STRING                { B.with_field_name  $2 }
//    | ALIAS IDENT                { B.with_field_alias $2 }
//    | field_source               { $1 } 

field_source:
    | SOURCE field_source_ref    { B.with_field_source $2 }

field_source_ref:
    | field_ns DOT fun_call      { B.fun_call ($1, $3) }

field_ns:
    | SQL                        { R.SQL }
    | IDENT                      { failwith (P.sprintf "UNKNOWN NAMESPACE (%s)" $1) }

fun_call:
    | IDENT OBR fun_args CBR    { ($1, $3) }

fun_args:                       { [] }
    | fun_arg COMMA fun_args    { $1 :: $3 }
    | fun_arg                   { $1 :: [] }

fun_arg:
    | IDENT                     { B.fun_arg_ident $1 }

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
    | FILTER filter_eq               { B.with_col_filt $2 }

filter_eq:
    | EQ filt_single_arg             { (R.EQ($2)) }
    | NE filt_single_arg             { (R.NE($2)) }
    | LT filt_single_arg             { (R.LT($2)) }
    | GT filt_single_arg             { (R.GT($2)) }
    | LE filt_single_arg             { (R.LE($2)) }
    | GE filt_single_arg             { (R.GE($2)) }
    | LIKE filt_like_arg             { (R.LIKE($2)) }
    | BETWEEN filt_bin_arg           { (R.BETWEEN($2)) }
    | filt_logic_op                  { $1 }

filt_single_arg:
    | OBR filt_arg CBR               { $2 }

filt_bin_arg:
    | OBR filt_arg COMMA filt_arg CBR { ($2, $4) }

filt_arg:
    | NUMBER                         { B.number_constant $1 }
    | STRING                         { B.string_constant $1 }

filt_like_arg:
    | OBR STRING CBR                 { B.string_constant $2 }


filt_logic_op:
    | OR  OBR filter_eq COMMA filter_eq CBR { (R.OR($3,$5))  }
    | AND OBR filter_eq COMMA filter_eq CBR { (R.AND($3,$5)) }
    | NOT OBR filter_eq CBR                 { (R.NOT($3)) }

%%
