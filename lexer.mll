{
  open Parser

  exception SyntaxError of string
}

  let digit = ['0'-'9']
  let frac = '.' digit*
  let exp = ['e' 'E'] ['-' '+']? digit+

  (* primitive values *)
  let int = '-'? digit digit*
  let float = digit* frac? exp?
  let char = ''' _ '''

  (* operators *)
  let arith = '+' | '-' | '*' | '/' | '%'
  let comp = '<' | '>' | "==" | "!=" | ">=" | "<="
  let logic = '!' | "&&" | "||"
  let bit = '&' | '|' | '^' | '~' | "<<" | ">>"
  let op = ((arith | bit) '='?) | comp | logic | '=' |"++" | "--" | '~'

  let white = [' ' '\t' '\n']+
  let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

  rule read = parse
    | white         { read lexbuf }

    (* values *)
    | int as lxm    { INT_VAL (int_of_string lxm) }
    | float as lxm  { FLOAT_VAL (float_of_string lxm) }
    | char as lxm   { CHAR_VAL (String.get lxm 1) }
    | "NULL"        { NULL }

    (* separators *)
    | '{'           { LEFT_BRACE }
    | '}'           { RIGHT_BRACE }
    | '('           { LEFT_PAREN }
    | ')'           { RIGHT_PAREN }
    | '['           { LEFT_BRACK }
    | ']'           { RIGHT_BRACK }
    | ';'           { SEMICOLON }
    | ','           { COMMA }
    | ':'           { COLON }
    | '?'           { QUERY }

    (* operators *)
    | op as lxm {
        try Hashtbl.find Operators.table id (* check if keyword *)
        with Not_found -> raise (SyntaxError ("Operator not found: " ^ lxm)
      }

    (* identifiers *)
    | id as lxm {
        try Hashtbl.find Keywords.table id (* check if keyword *)
        with Not_found -> ID lxm
      }

    | _ as lxm      { raise (SyntaxError ("Unexpected char: " ^ lxm)) }
    | eof           { EOF }
