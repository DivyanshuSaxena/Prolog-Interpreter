type token =
  | Var of (string)
  | Id of (string)
  | Const of (int)
  | Bool of (bool)
  | COMMA
  | SEP
  | OPAREN
  | CPAREN
  | PLUS
  | MINUS
  | PROD
  | DIV
  | MOD
  | EXP
  | EQ
  | GT
  | LT
  | GTE
  | LTE
  | AND
  | OR
  | NOT
  | LISTSEP
  | OSQUARE
  | CSQUARE
  | CUT
  | FAIL
  | QUERY
  | END
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Interpreter.program
val goal :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Interpreter.clause
