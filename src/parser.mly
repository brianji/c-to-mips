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

/* if-else ambiguity */
%nonassoc IFX
%nonassoc ELSE

%start prog
%type <Ast.prog> prog
%%
/* C file */
prog:
  | prog_elmts EOF { $1 }
  ;
prog_elmts:
  | { [] }
  | prog_elmt prog_elmts { $1 :: $2 }
  ;
prog_elmt:
  | func { Func $1 }
  | dec_stmt { Global $1 }
  ;

func:
  | prim ID LEFT_PAREN params RIGHT_PAREN block { ($1, $2, $4, $6) }
  ;
params:
  | { [] }
  | prim ID { [$1, $2] }
  | prim ID COMMA params { ($1, $2) :: $4 }
  ;

var:
  | ID { $1 }
  ;
prim:
  | VOID { Void }
  | CHAR { Char }
  | INT { Int }
  | FLOAT { Float }
  ;
value:
  | INT_VAL { IntVal $1 }
  | FLOAT_VAL { FloatVal $1 }
  | CHAR_VAL { CharVal $1 }
  ;

/* Statements */
statements:
  | { [] }
  | statement statements { $1 :: $2 }
  ;
statement:
  | dec_stmt { Dec $1 }
  | expr SEMICOLON { Expr $1 }
  | return_statement { $1 }
  | BREAK SEMICOLON { Break }
  | CONTINUE SEMICOLON { Continue }
  | block { $1 }
  | FOR LEFT_PAREN for_control RIGHT_PAREN statement { For ($3, $5) }
  | WHILE condition statement { While ($2, $3) }
  | IF condition statement %prec IFX { If ($2, $3) }
  | IF condition statement ELSE statement { IfElse ($2, $3, $5) }
  ;
dec_stmt:
  | prim decs SEMICOLON { ($1, $2) }
  ;
decs:
  | { [] }
  | dec { [$1] }
  | dec COMMA decs { $1 :: $3 }
  ;
dec:
  | var { Var $1 }
  | var ASSIGN expr14 { Assign ($1, Asgmt, $3) }
  ;
return_statement:
  | RETURN expr SEMICOLON { ReturnExpr $2 }
  | RETURN SEMICOLON { Return }
  ;
block:
  | LEFT_BRACE statements RIGHT_BRACE { Block $2 }
  ;
for_control:
  | for_expr SEMICOLON for_expr SEMICOLON for_expr { ($1, $3, $5) }
  ;
for_expr:
  | expr { $1 }
  | { Empty }
  ;
condition:
  | LEFT_PAREN expr RIGHT_PAREN { $2 }
  ;

/*
 * Expressions and operators. Follows order of operations. Ordered from highest
 * to lowest precedence.
 */
expr:
  | expr15 { $1 }
  ;
expr0:
  | var { Var $1 }
  | value { Value $1 }
  | LEFT_PAREN expr RIGHT_PAREN { Paren ($2) }
  ;
expr1: /* Function calls, postfix */
  | function_call { $1 }
  | expr0 op1 { Postfix ($1, $2) }
  | expr0 { $1 }
  ;
function_call:
  | ID LEFT_PAREN args RIGHT_PAREN { FunctionCall ($1, $3) }
  ;
args:
  | { [] }
  | expr14 { [$1] }
  | expr14 COMMA args { $1 :: $3 }
  ;
op1:
  | INC { Incrmt }
  | DEC { Decrmt }
  ;
expr2: /* Prefix */
  | op2 expr1 { Prefix ($1, $2) }
  | expr1 { $1 }
  ;
op2:
  | INC { Incrmt }
  | DEC { Decrmt }
  | NOT { Not }
  | COMP { Comp }
  | PLUS { Pos }
  | MINUS { Neg }
  ;
expr3: /* Multiplication, division, modulus */
  | expr3 op3 expr2 { Infix ($1, $2, $3) }
  | expr2 { $1 }
  ;
op3:
  | TIMES { Times }
  | DIVIDE { Divide }
  | MOD { Mod }
  ;
expr4: /* Addition, subtraction */
  | expr4 op4 expr3 { Infix ($1, $2, $3) }
  | expr3 { $1 }
  ;
op4:
  | PLUS { Plus }
  | MINUS { Minus }
  ;
expr5: /* Bit-shift */
  | expr5 op5 expr4 { Infix ($1, $2, $3 ) }
  | expr4 { $1 }
  ;
op5:
  | SHIFT_LEFT { ShiftLeft }
  | SHIFT_RIGHT { ShiftRight }
  ;
expr6: /* Comparison */
  | expr6 op6 expr5 { Infix ($1, $2, $3 ) }
  | expr5 { $1 }
  ;
op6:
  | LESS { Less }
  | LESSER_EQ { LesserEq }
  | GREATER { Greater }
  | GREATER_EQ { GreaterEq }
  ;
expr7: /* Equality */
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
expr14: /* Assignment */
  | var op14 expr14 { Assign ($1, $2, $3) }
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
