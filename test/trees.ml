open Ast

(* ASTs used for unit testing *)

let basic = [
  Func (Void, "f", [], Block []);
  Global (Int, [Var "x"]);
  Func (Int, "main", [], Block [ReturnExpr (Value (IntVal 5))])
]

let arith =
  let block = Block [
      ReturnExpr (
        Infix (
          Infix (
            Infix (
              Value (IntVal 1),
              Plus,
              Infix (
                Value (IntVal 2),
                Times,
                Prefix (Neg, Value (IntVal 3))
              )
            ),
            Plus,
            Infix (
              Paren (
                Infix (
                  Value (IntVal 4),
                  ShiftLeft,
                  Value (IntVal 1)
                )
              ),
              Divide,
              Value (IntVal 4)
            )
          ),
          Plus,
          Paren (
            Infix (
              Infix (
                Value (IntVal 1),
                And,
                Value (IntVal 0)
              ),
              Or,
              Infix (
                Value (IntVal 1),
                Equals,
                Value (IntVal 0)
              )
            )
          )
        )
      )
    ]
  in
  [Func (Int, "main", [], block)]

let dec = 
  let block = Block [
      Dec (
        Int,
        [
          Var "x";
          Assign (
            "y",
            Asgmt,
            Value (IntVal 0)
          )
        ]
      )
    ]
  in
  [Func (Int, "main", [], block)]

let call = [
  Func (
    Int,
    "f",
    [(Int, "x"); (Int, "y")],
    Block [
      ReturnExpr (
        Infix (
          Var "x",
          Plus,
          Var "y"
        )
      )
    ]
  );
  Func (
    Int,
    "main",
    [],
    Block [
      ReturnExpr (
        FunctionCall (
          "f",
          [Value (IntVal 1); Value (IntVal 2)]
        )
      )
    ]
  )
]

let ifelse = [
  Func (
    Int,
    "main",
    [],
    Block [
      If (
        Value (IntVal 1),
        Block []
      );
      IfElse (
        Value (IntVal 2),
        Block [],
        IfElse (
          Value (IntVal 3),
          Block [],
          Block []
        )
      );
      If (
        Value (IntVal 4),
        IfElse (
          Value (IntVal 5),
          Block [],
          Block []
        )
      )
    ]
  )
]
