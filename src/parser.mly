%{
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token DOT
%token FIELD COLUMN END ALIAS NAME SOURCE FILTER SORT FOLD
%token NONE YES NO NONE ASC DESC
%token CONNECTION DATASOURCE TABLE
%token EOF

%start toplevel
%type <Ast.report> toplevel

%%

toplevel:
    | entries               { Ast.report $1 }
    | EOF                   { assert false  }

entries:                    { [] }
    | entry entries         { $1 :: $2 }

entry:
    | field                 { Ast.f $1 }
    | column                { Ast.c $1 }
    | connection            { Ast.conn $1 }
    | datasource            { Ast.ds $1 }

column:
    | COLUMN column_attribs END  { Ast.column $2 }

column_attribs:                     { [] }
    | column_attrib column_attribs  { $1 :: $2 }

column_attrib:
    | ALIAS  IDENT               { Ast.alias $2 }
    | NAME   STRING              { Ast.name  $2 }
    | SOURCE col_ref             { Ast.col_source $2 }
    | FILTER IDENT               { failwith "FILTER is not supported yet" }
    | SORT   sort_args           { Ast.sort $2  }
    | FOLD   fold_args           { Ast.fold $2  }

sort_args:
    | ASC                        { Ast.ASC  }
    | DESC                       { Ast.DESC }

fold_args:
    | YES                        { Ast.fold_yes () }
    | NO                         { Ast.fold_no  () }


col_ref:
    | IDENT DOT IDENT            { Ast.col_ref $1 $3 }


field:
    | FIELD END                  { failwith "FIELD is not supported yet" }

connection:
    | CONNECTION IDENT STRING    { Ast.connection $2 $3 }

datasource:
    | DATASOURCE TABLE datasource_args END  { Ast.datasource_table $3 }

datasource_args:                      { [] }
    | datasource_arg datasource_args  { $1 :: $2 }

datasource_arg:
    | SOURCE IDENT                    { Ast.ds_source $2 }
    | ALIAS  IDENT                    { Ast.ds_alias  $2 }

%%
