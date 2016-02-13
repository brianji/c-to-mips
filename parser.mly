%{
    open Ast
%}
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> ID
%token NULL
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token COMMA
%token EOF

%start main
%type <Ast.prog> main
%%
