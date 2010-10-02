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
    | entries               { $1 }
    | EOF                   { print_endline "eof!" ; () }


entries:                    { print_endline "empty!" }
    | entry entries         { $1; print_endline "rest of entries" }

entry:
    | field                 { $1 }
    | column                { $1 }

column:
    | COLUMN column_attribs END  { print_endline "column!" }

column_attribs:                  { () }
    | column_attrib column_attribs   { $1; print_endline "column_attribs" }

column_attrib:
    | ALIAS  IDENT               { Printf.printf "(alias %s)\n" $2 }
    | NAME   STRING              { Printf.printf "(name %s)\n" $2 }
    | SOURCE IDENT               { Printf.printf "(source %s)\n" $2 }
    | FILTER IDENT               { Printf.printf "(filter %s)\n" $2 }
    | SORT   sort_args           { Printf.printf "(sort)\n"  }
    | FOLD   fold_args           { Printf.printf "(fold)\n" }

sort_args:
    | ASC                        { Printf.printf "(sort_asc)\n" }
    | DESC                       { Printf.printf "(sort_desc)\n" }
    | NONE                       { Printf.printf "(sort_none)\n" }

fold_args:
    | YES                        { Printf.printf "(fold_yes)\n" }
    | NO                         { Printf.printf "(fold_no)\n" }

field:
    | FIELD END             { print_endline "field!" }

%%
