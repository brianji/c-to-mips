open Parser

let table = Hashtbl.create 100
let _ =
  List.iter
    (fun (kwd, tok) -> Hashtbl.add table kwd tok)
    [
      "auto", AUTO;
      "break", BREAK;
      "case", CASE;
      "char", CHAR;
      "const", CONST;
      "continue", CONTINUE;
      "default", DEFAULT;
      "do", DO;
      "double", DOUBLE;
      "else", ELSE;
      "enum", ENUM;
      "extern", EXTERN;
      "float", FLOAT;
      "for", FOR;
      "goto", GOTO;
      "if", IF;
      "int", INT;
      "long", LONG;
      "register", REGISTER;
      "return", RETURN;
      "short", SHORT;
      "signed", SIGNED;
      "sizeof", SIZEOF;
      "static", STATIC;
      "struct", STRUCT;
      "switch", SWITCH;
      "typedef", TYPEDEF;
      "union", UNION;
      "unsigned", UNSIGNED;
      "void", VOID;
      "volatile", VOLATILE;
      "while", WHILE
    ]
