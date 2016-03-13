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

%nonassoc IFX
%nonassoc ELSE

%start main
%type <Ast.func> main
%%
main: func EOF { $1 }
  ;
func:
  | return ID LEFT_PAREN params RIGHT_PAREN block { ($1, $2, $4, $6) }
  ;
block:
  | LEFT_BRACE statements RIGHT_BRACE { Block $2 }
  ;
condition:
  | LEFT_PAREN expr RIGHT_PAREN { $2 }
  ;
return:
  | VOID { Void }
  | prim { Prim $1 }
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
  | prim expr SEMICOLON { Dec ($1, $2) }
  | expr SEMICOLON { Expr $1 }
  | return_statement { $1 }
  | BREAK SEMICOLON { Break }
  | CONTINUE SEMICOLON { Continue }
  | block { $1 }
  | WHILE condition statement { While ($2, $3) }
  | FOR LEFT_PAREN for_control RIGHT_PAREN statement { For ($3, $5) }
  | IF condition statement %prec IFX { If ($2, $3) }
  | IF condition statement ELSE statement { IfElse ($2, $3, $5) }
  ;
return_statement:
  | RETURN expr SEMICOLON { ReturnExpr $2 }
  | RETURN SEMICOLON { Return }
  ;
for_control:
  | for_expr SEMICOLON for_expr SEMICOLON for_expr { ($1, $3, $5) }
  ;
for_expr:
  | expr { $1 }
  | { Empty }
  ;
expr:
  | expr15 { $1 }
  ;
var:
  | ID { $1 }
  ;
expr0:
  | var { Var $1 }
  | value { Value $1 }
  | LEFT_PAREN expr RIGHT_PAREN { Paren ($2) }
  ;
expr1:
  | var op1 { Postfix ($1, $2) }
  | expr0 { $1 }
  ;
op1:
  | INC { Incrmt }
  | DEC { Decrmt }
  ;
expr2:
  | op2 var { Prefix ($1, $2) }
  | expr1 { $1 }
  ;
op2:
  | INC { Incrmt }
  | DEC { Decrmt }
  ;
expr3:
  | expr3 op3 expr2 { Infix ($1, $2, $3) }
  | expr2 { $1 }
  ;
op3:
  | TIMES { Times }
  | DIVIDE { Divide }
  | MOD { Mod }
  ;
expr4:
  | expr4 op4 expr3 { Infix ($1, $2, $3) }
  | expr3 { $1 }
  ;
op4:
  | PLUS { Plus }
  | MINUS { Minus }
  ;
expr5:
  | expr5 op5 expr4 { Infix ($1, $2, $3 ) }
  | expr4 { $1 }
  ;
op5:
  | SHIFT_LEFT { ShiftLeft }
  | SHIFT_RIGHT { ShiftRight }
  ;
expr6:
  | expr6 op6 expr5 { Infix ($1, $2, $3 ) }
  | expr5 { $1 }
  ;
op6:
  | LESS { Less }
  | LESSER_EQ { LesserEq }
  | GREATER { Greater }
  | GREATER_EQ { GreaterEq }
  ;
expr7:
  | expr7 op7 expr6 { Infix ($1, $2, $3 ) }
  | expr6 { $1 }
  ;
op7:
  | EQUALS { Equals }
  | NOT_EQUALS { NotEquals }
  ;
expr8:
  | expr8 BIT_AND expr7 { Infix ($1, BitAnd, $3 ) }
  | expr7 { $1 }
  ;
expr9:
  | expr9 BIT_XOR expr8 { Infix ($1, BitXor, $3 ) }
  | expr8 { $1 }
  ;
expr10:
  | expr10 BIT_OR expr9 { Infix ($1, BitOr, $3 ) }
  | expr9 { $1 }
  ;
expr11:
  | expr11 AND expr10 { Infix ($1, And, $3) }
  | expr10 { $1 }
  ;
expr12:
  | expr12 OR expr11 { Infix ($1, Or, $3) }
  | expr11 { $1 }
  ;
expr13:
  | expr12 { $1 } /* TODO: ternary operator */
  ;
expr14:
  | var op14 expr14 { Infix (Var $1, $2, $3) }
  | expr13 { $1 }
  ;
op14:
  | ASSIGN { Asgmt }
  | PLUS_A { PlusA }
  | MINUS_A { MinusA }
  | TIMES_A { TimesA }
  | DIVIDE_A { DivideA }
  | MOD_A { ModA }
  | SHIFT_LEFT_A { ShiftLeftA }
  | SHIFT_RIGHT_A { ShiftRightA }
  | BIT_AND_A { BitAndA }
  | BIT_OR_A { BitOrA }
  | BIT_XOR_A { BitXorA }
  ;
expr15:
  | expr15 COMMA expr14 { Infix ($1, Comma, $3) }
  | expr14 { $1 }
  ;
