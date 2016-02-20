open To_string

let print_params params =
  let param_strings = List.map param_string params in
  let rec aux = function
    | [] -> ()
    | [h] -> print_string h
    | h :: t ->
      print_string (h ^ ", ");
      aux t
  in
  aux param_strings

let print_statement = ()

let print_statements = ()

let print_function f =
  let (return, id, params, statements) = f in
  print_string (return_string return);
  print_string " ";
  print_string id;
  print_string "(";
  print_params params;
  print_string ") {";
  print_newline ();
  print_statements;
  print_string "}";
  print_newline ()

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let f = Parser.main Lexer.read lexbuf in
  print_function f;
  exit 0
