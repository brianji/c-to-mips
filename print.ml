open Parser

let token_string token =
  let str = match token with
  | INT_VAL i -> string_of_int i
  | FLOAT_VAL f -> string_of_float f
  | CHAR_VAL c -> String.make 1 c
  | NULL -> "NULL"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACK -> "["
  | RIGHT_BRACK -> "]"
  | SEMICOLON -> ";"
  | COMMA -> ","
  | COLON -> ":"
  | QUERY -> "?"
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIVIDE -> "/"
  | MOD -> "%"
  | INC -> "++"
  | DEC -> "--"
  | EQUALS -> "=="
  | NOT_EQUALS -> "!="
  | GREATER -> ">"
  | LESS -> "<"
  | GREATER_EQ -> ">="
  | LESSER_EQ -> "<="
  | AND -> "&&"
  | OR -> "||"
  | NOT -> "!"
  | BIT_AND -> "&"
  | BIT_OR -> "|"
  | BIT_XOR -> "^"
  | COMP -> "~"
  | SHIFT_LEFT -> "<<"
  | SHIFT_RIGHT -> ">>"
  | ASSIGN -> "="
  | PLUS_A -> "+="
  | MINUS_A -> "-="
  | TIMES_A -> "*="
  | DIVIDE_A -> "/="
  | MOD_A -> "%="
  | SHIFT_LEFT_A -> "<<="
  | SHIFT_RIGHT_A -> ">>="
  | BIT_AND_A -> "&="
  | BIT_OR_A -> "|="
  | BIT_XOR_A -> "^="
  | INCLUDE -> "#include"
  | DEFINE -> "#define"
  | UNDEF -> "#undef"
  | IF_D -> "#if"
  | IFDEF -> "#ifdef"
  | IFNDEF -> "#ifndef"
  | ERROR -> "#error"
  | ID i -> i
  | AUTO -> "auto"
  | BREAK -> "break"
  | CASE -> "case"
  | CHAR -> "char"
  | CONST -> "const"
  | CONTINUE -> "continue"
  | DEFAULT -> "default"
  | DO -> "do"
  | DOUBLE -> "double"
  | ELSE -> "else"
  | ENUM -> "enum"
  | EXTERN -> "extern"
  | FLOAT -> "float"
  | FOR -> "for"
  | GOTO -> "goto"
  | IF -> "if"
  | INT -> "int"
  | LONG -> "long"
  | REGISTER -> "register"
  | RETURN -> "return"
  | SHORT -> "short"
  | SIGNED -> "signed"
  | SIZEOF -> "sizeof"
  | STATIC -> "static"
  | STRUCT -> "struct"
  | SWITCH -> "switch"
  | TYPEDEF -> "typedef"
  | UNION -> "union"
  | UNSIGNED -> "unsigned"
  | VOID -> "void"
  | VOLATILE -> "volatile"
  | WHILE -> "while"
  | _ -> ""
  in str ^ " "

let read_tokens buff =
  let rec process acc =
    let token = Lexer.read buff in
    if token = EOF then
      List.rev acc
    else
      process (token :: acc)
  in
  process []

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let tokens = read_tokens lexbuf in
  let strings = List.map token_string tokens in
  List.fold_left (fun _ s -> print_string s) () strings;
  print_newline(); flush stdout;
  exit 0;;
