open Parser

let table = Hashtbl.create 200
let _ =
  List.iter
    (fun (kwd, tok) -> Hashtbl.add table kwd tok)
    [
      (* arithmetic *)
      "+", PLUS;
      "-", MINUS;
      "*", TIMES;
      "/", DIVIDE;
      "%", MOD;
      "++", INC;
      "--", DEC;

      (* comparison *)
      "==", EQUALS;
      "!=", NOT_EQUALS;
      ">", GREATER;
      "<", LESS;
      ">=", GREATER_EQ;
      "<=", LESSER_EQ;

      (* logical *)
      "&&", AND;
      "||", OR;
      "!", NOT;

      (* bitwise *)
      "&", BIT_AND;
      "|", BIT_OR;
      "^", BIT_XOR;
      "~", COMP;
      "<<", SHIFT_LEFT;
      ">>", SHIFT_RIGHT;

      (* assignment *)
      "=", ASSIGN;
      "+=", PLUS_A;
      "-=", MINUS_A;
      "*=", TIMES_A;
      "/=", DIVIDE_A;
      "%=", MOD_A;
      "<<=", SHIFT_LEFT_A;
      ">>=", SHIFT_RIGHT_A;
      "&=", BIT_AND_A;
      "|=", BIT_OR_A;
      "^=", BIT_XOR_A
    ]
