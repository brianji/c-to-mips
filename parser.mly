%{
%}
/* values */
%token <int> INT_VAL
%token <float> FLOAT_VAL
%token <char> CHAR_VAL
%token NULL

/* separators */
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK
%token SEMICOLON
%token COMMA
%token COLON
%token QUERY

/* operators */
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token INC
%token DEC

/* comparison */
%token EQUALS
%token NOT_EQUALS
%token GREATER
%token LESS
%token GREATER_EQ
%token LESSER_EQ

/* logical */
%token AND
%token OR
%token NOT

/* bitwise */
%token BIT_AND
%token BIT_OR
%token BIT_XOR
%token COMP
%token SHIFT_LEFT
%token SHIFT_RIGHT

/* assignment */
%token ASSIGN
%token PLUS_A
%token MINUS_A
%token TIMES_A
%token DIVIDE_A
%token MOD_A
%token SHIFT_LEFT_A
%token SHIFT_RIGHT_A
%token BIT_AND_A
%token BIT_OR_A
%token BIT_XOR_A

/* directives */
%token INCLUDE
%token DEFINE
%token UNDEF
%token IF_D
%token IFDEF
%token IFNDEF
%token ERROR

/* identifiers */
%token <string> ID
%token AUTO
%token BREAK
%token CASE
%token CHAR
%token CONST
%token CONTINUE
%token DEFAULT
%token DO
%token DOUBLE
%token ELSE
%token ENUM
%token EXTERN
%token FLOAT
%token FOR
%token GOTO
%token IF
%token INT
%token LONG
%token REGISTER
%token RETURN
%token SHORT
%token SIGNED
%token SIZEOF
%token STATIC
%token STRUCT
%token SWITCH
%token TYPEDEF
%token UNION
%token UNSIGNED
%token VOID
%token VOLATILE
%token WHILE

%token EOF

%start main
%type <unit> main
%%
main:
  | EOF {}
  ;
