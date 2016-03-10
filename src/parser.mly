%{
  open Ast
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
%type <Ast.func> main
%%
main: func EOF { $1 }
  ;
func:
  | return ID LEFT_PAREN params RIGHT_PAREN LEFT_BRACE statements RIGHT_BRACE
      { ($1, $2, $4, $7) }
  ;
return:
  | VOID { Void }
  | prim { Prim $1 }
  ;
params:
  | { [] }
  | prim ID { [$1, $2] }
  | prim ID COMMA params { ($1, $2) :: $4 }
  ;
statements:
  | { [] }
  | statement statements { $1 :: $2 }
  ;
statement:
  | decl { $1 }
  | expr SEMICOLON { Expr $1 }
  ;
decl:
  | prim dec_exprs SEMICOLON { Dec ($1, $2) }
  ;
dec_exprs:
  | { [] }
  | dec_expr { [$1] }
  | dec_expr COMMA dec_exprs { $1 :: $3}
  ;
dec_expr:
  | ID { DecVar $1 }
  | ID ASSIGN expr { InitVar ($1, $3) }
  ;
expr:
  | var { Var $1 }
  | value { Value $1 }
  | infix { $1 }
  | prefix { $1 }
  | postfix { $1 }
  ;
infix: expr inop expr { Infix ($1, $2, $3) };
inop:
  | PLUS { Plus }
  | MINUS { Minus }
  | TIMES { Times }
  | DIVIDE { Divide }
  | MOD { Mod }
prefix:
  | INC var { Prefix (Incrmt, $2) }
  | DEC var { Prefix (Decrmt, $2) }
  ;
postfix:
  | var INC { Postfix ($1, Incrmt) }
  | var DEC { Postfix ($1, Decrmt) }
  ;
var: ID { $1 }
  ;
prim:
  | CHAR { Char }
  | INT { Int }
  | FLOAT { Float }
  ;
value:
  | INT_VAL { Integer $1 }
  | FLOAT_VAL { Decimal $1 }
  | CHAR_VAL { Letter $1 }
  ;
