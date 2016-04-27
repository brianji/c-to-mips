open Parser

let table = Hashtbl.create 25
let _ =
  List.iter
    (fun (kwd, tok) -> Hashtbl.add table kwd tok)
    [
      "#include", INCLUDE;
      "#define", DEFINE;
      "#undef", UNDEF;
      "#if", IF_D;
      "#ifdef", IFDEF;
      "#ifndef", IFNDEF;
      "#error", ERROR
    ]
