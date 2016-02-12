{
  open Parser

  exception SyntaxError of string
}

  let digit = ['0'-'9']
  let frac = '.' digit*
  let exp = ['e' 'E'] ['-' '+']? digit+

  (* primitives *)
  let int = '-'? digit digit*
  let float = digit* frac? exp?
  let char = ''' _ '''

  let white = [' ' '\t' '\n']+
  let newline = '\r' | '\n' | "\r\n"
  let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

  rule read =
    parse
    | white         { read lexbuf }
    | int as lxm    { INT (int_of_string lxm) }
    | float as lxm  { FLOAT (float_of_string lxm) }
    | char as lxm   { CHAR (char_of_string lxm) }
    | "NULL"        { NULL }
    | '{'           { LEFT_BRACE }
    | '}'           { RIGHT_BRACE }
    | '('           { LEFT_PAREN }
    | ')'           { RIGHT_PAREN }
    | ';'           { SEMICOLON }
    | ','           { COMMA }
    | id            { ID (Lexing.lexeme lexbuf) }
    | _ as lxm      { raise (SyntaxError ("Unexpected char: " ^ lxm)) }
    | eof           { EOF }
