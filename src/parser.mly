%{
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token FIELD COLUMN END ALIAS NAME SOURCE FILTER SORT FOLD
%token NONE YES NO NONE ASC DESC
%token EOF

%start toplevel
%type <Ast.ast> toplevel

%%

toplevel:
    | entries               { assert false }
    | EOF                   { assert false }

entries:                    { assert false }
    | entry entries         { assert false }

entry:
    | field                 { assert false }
    | column                { assert false }

column:
    | COLUMN column_attribs END  { Ast.column $2 }

column_attribs:                     { [] }
    | column_attrib column_attribs  { $1 :: [] }

column_attrib:
    | ALIAS  IDENT               { Ast.alias $2 }
    | NAME   STRING              { Ast.name  $2 }
    | SOURCE IDENT               { assert false }
    | FILTER IDENT               { assert false }
    | SORT   sort_args           { Ast.sort $2  }
    | FOLD   fold_args           { Ast.fold $2  }

sort_args:
    | ASC                        { Some(Ast.ASC)  }
    | DESC                       { Some(Ast.DESC) }
    | NONE                       { None }

fold_args:
    | YES                        { Ast.fold_yes () }
    | NO                         { Ast.fold_no  () }

field:
    | FIELD END                  { failwith "FIELD is not supported yet" }

%%
