{
  open Parser

  exception SyntaxError of string
}

let white = [' ' '\t' '\n']+
let word = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
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

rule read = parse
  | white { read lexbuf }

  (* values *)
  | int as lxm { INT_VAL (int_of_string lxm) }
  | float as lxm { FLOAT_VAL (float_of_string lxm) }
  | char as lxm { CHAR_VAL (String.get lxm 1) }
  | "NULL" { NULL }

  (* separators *)
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '[' { LEFT_BRACK }
  | ']' { RIGHT_BRACK }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | ':' { COLON }
  | '?' { QUERY }

  (* operators *)
  | op as lxm {
      try Hashtbl.find Operators.table lxm (* check if keyword *)
      with Not_found -> raise (SyntaxError ("Invalid operator: " ^ lxm))
    }

  (* directives *)
  | ('#' word) as lxm {
      try Hashtbl.find Directives.table lxm (* check if keyword *)
      with Not_found -> raise (SyntaxError ("Invalid directive: " ^ lxm))
    }

  (* keywords and identifiers *)
  | word as lxm {
      try Hashtbl.find Keywords.table lxm (* check if keyword *)
      with Not_found -> ID lxm
    }

  (* comments *)
  | "//" { singleline_comment lexbuf }
  | "/*" { multiline_comment lexbuf }

  | _ as lxm { raise (SyntaxError ("Unexpected char: " ^ lxm)) }
  | eof { EOF }
and singleline_comment = parse
  | "\n" { read lexbuf }
  | _ { singleline_comment lexbuf }
and multiline_comment = parse
  | "*/" { read lexbuf }
  | _ { multiline_comment lexbuf }
