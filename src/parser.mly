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
    | COLUMN column_attribs END  { assert false }

column_attribs:                  { assert false }
    | column_attrib column_attribs  { assert false }

column_attrib:
    | ALIAS  IDENT               { assert false }
    | NAME   STRING              { assert false }
    | SOURCE IDENT               { assert false }
    | FILTER IDENT               { assert false }
    | SORT   sort_args           { assert false }
    | FOLD   fold_args           { assert false }

sort_args:
    | ASC                        { assert false }
    | DESC                       { assert false }
    | NONE                       { assert false }

fold_args:
    | YES                        { assert false }
    | NO                         { assert false }

field:
    | FIELD END                  { assert false }

%%
