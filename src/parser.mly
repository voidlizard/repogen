%{
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token FIELD END
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
    | FIELD END             { print_endline "field entry" }

%%

