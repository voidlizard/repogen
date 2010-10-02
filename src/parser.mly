%{
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token FIELD COLUMN END
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
    | COLUMN END            { print_endline "column!" }


field:
    | FIELD END             { print_endline "field!" }

%%
