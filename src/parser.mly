%{

module B = Report_builder
module R = Report
module P = Printf

%}

%token DOLLAR OBR CBR OBRACE CBRACE
%token <int> INT
%token <string> STRING
%token <string> NUMBER
%token <string> IDENT
%token ASTERISK COLON
%token DOT COMMA
%token FIELD COLUMN VARIABLE SET AS END VALUE ALIAS NAME SOURCE FILTER SORT FOLD
%token GROUP
%token NONE YES NO NONE ASC DESC
%token CONNECTION DATASOURCE TABLE FUNCTION
%token TEMPLATE TEMPLATE_DIRS
%token OUTPUT FILE TEMPORARY STDOUT
%token POSTPROCESS ECHO ABORT DROP
%token BEFORE AFTER LAST FIRST NULL NULLS
%token EQ NE LT GT LE GE 
%token OR AND NOT
%token IN BETWEEN BY 
%token SQL
%token LIKE

%token EOF

%left DOT

%start toplevel
%type <Report.report_t> toplevel

%%

toplevel:
    | entries               { B.build_report $1 }
    | EOF                   { assert false  }

entries:                    { [] }
    | entry entries         { $1 :: $2 }

entry:
    | field                 { $1 }
    | template              { $1 }
    | template_dirs         { $1 }
    | column                { B.with_column $1 }
    | connection            { B.with_connection $1 }
    | datasource            { B.with_datasource $1 }
    | output                { $1 }
    | postprocess           { $1 }
    | misc_actions          { $1 }
    | var_def1              { $1 } 

column:
    | COLUMN column_attribs END  { $2 }

column_attribs:                     { [] }
    | column_attrib column_attribs  { $1 :: $2 }

column_attrib:
    | ALIAS  IDENT               { B.with_col_alias $2 }
    | NAME   STRING              { B.with_col_name $2  }
    | SOURCE source              { B.with_col_source $2 }
    | filter                     { $1 }
    | SORT  sort_args            { B.with_col_order $2 }
    | SORT FOLD sort_args        { B.with_col_order ((B.col_fold ()) :: $3) }
    | GROUP                      { B.with_group }

sort_args:                        { [] }
    | sort_order                  { [$1] }
    | sort_order sort_nulls       { [$1; $2] }

sort_nulls:
    | NULLS FIRST                { B.col_nulls_first () }
    | NULLS LAST                 { B.col_nulls_last () }

sort_order:
    | ASC                        { B.col_order_asc ()  }
    | DESC                       { B.col_order_desc () }

source:
    | col_ref                    { $1 }
    | field_source_ref           { B.col_fun_call $1 }

col_ref:
    | IDENT DOT IDENT            { B.col_ref $1 $3 }

table_ref:
    | IDENT DOT ASTERISK         { B.table_ref $1 }

field:
    | FIELD field_decl END       { B.with_field $2 }

field_decl:                      { [] }
    | field_entry field_decl     { $1 :: $2 }

field_entry:
    | ALIAS IDENT                { B.with_field_alias $2 }
    | FILTER filt_by filter_eq   { B.with_field_filter $2 $3 }
    | field_source               { $1 } 

field_source:
    | SOURCE field_source_ref    { B.with_field_source $2 }

field_source_ref:
    | field_ns DOT fun_call      { B.fun_call ($1, $3) }

field_ns:
    | SQL                        { R.SQL }
/*    | IDENT                    { failwith (P.sprintf "UNKNOWN NAMESPACE (%s)" $1) } --- makes conflicts */

fun_call:
    | IDENT OBR fun_args CBR    { ($1, $3) }

fun_args:                       { [] }
    | fun_arg COMMA fun_args    { $1 :: $3 }
    | fun_arg                   { $1 :: [] }

fun_arg:
    | IDENT                     { B.fun_arg_ident $1 }
    | STRING                    { B.fun_arg_str $1 }
    | NUMBER                    { B.fun_arg_num $1 }
    | col_ref                   { B.fun_arg_col_ref $1 }
    | table_ref                 { B.fun_arg_table_ref $1 }
    | field_source_ref          { B.fun_arg_src $1 }
    | var_ref                   { B.fun_arg_var $1 }

filt_by:
    | BY OBR IDENT CBR          { $3 }

template:
    | TEMPLATE STRING            { B.with_template $2 }

template_dirs:
    | TEMPLATE_DIRS STRING       { B.with_template_dirs $2 }

connection:
    | CONNECTION IDENT STRING    { ( $2, $3) }

datasource:
    | DATASOURCE TABLE datasource_args END  { B.with_datasource_table $3 }
    | DATASOURCE FUNCTION datasource_fun_args END  { B.with_datasource_function $3 }

datasource_args:                 { [] }
    | datasource_arg datasource_args { $1 :: $2 }

datasource_fun_args:                 { [] }
    | datasource_fun_arg datasource_fun_args { $1 :: $2 }

datasource_arg:
    | ALIAS  IDENT                { B.datasource_alias $2  }
    | SOURCE IDENT                { B.datasource_source $2 }

datasource_fun_arg:
    | ALIAS  IDENT                { B.datasource_fun_alias $2 }
    | SOURCE field_source_ref     { B.datasource_fun_source $2 } 

output:
    | OUTPUT TEMPORARY               { B.with_output_temp () }
    | OUTPUT TEMPORARY STRING STRING { B.with_output_temp ~prefix:$3 ~suffix:$4 () }
    | OUTPUT STDOUT                  { B.with_output_stdout () }
    | OUTPUT FILE STRING             { B.with_output_file $3 }

postprocess:
    | POSTPROCESS STRING             { B.with_postprocess $2 }
    | POSTPROCESS DROP               { B.with_postprocess_drop () }


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
    | source                         { B.var_filt_arg $1 }
    | var_ref                        { $1 }

filt_like_arg:
    | OBR STRING CBR                 { B.string_constant $2 }
    | OBR source CBR                 { B.var_filt_arg $2 }
    | OBR var_ref CBR                { $2 }

filt_logic_op:
    | OR  OBR filter_eq COMMA filter_eq CBR { (R.OR($3,$5))  }
    | AND OBR filter_eq COMMA filter_eq CBR { (R.AND($3,$5)) }
    | NOT OBR filter_eq CBR                 { (R.NOT($3)) }

var_ref:
    DOLLAR OBRACE IDENT CBRACE              { B.var_ref $3 }

var_def1:
    VARIABLE var_def1_args END              { B.with_var_def $2 }

var_def1_args:                              { [] }
    | var_def1_arg var_def1_args            { $1 :: $2 }

var_def1_arg:
    | NAME STRING                           { B.with_tmp_var_name $2 }
    | ALIAS IDENT                           { B.with_tmp_var_alias $2 }
    | VALUE STRING                          { B.with_tmp_var_value $2 }

%%
