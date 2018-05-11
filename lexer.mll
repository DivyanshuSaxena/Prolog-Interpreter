{
open Parser        (* The type token is defined in parser.mly *)
open Printf
exception Eof
}
let eol = '\n'

rule token = parse
    [' ' '\t']          { token lexbuf } 
  | ['0'-'9']+ as num   { let () = Printf.printf "Const" in Const(int_of_string num) }
  | "true"              { let () = Printf.printf "True" in Bool(true) }
  | "false"             { let () = Printf.printf "False" in Bool(false) }
  | '+'                 { let () = Printf.printf "+" in PLUS }
  | '-'                 { let () = Printf.printf "-" in MINUS }
  | '*'                 { let () = Printf.printf "*" in PROD }
  | '/'                 { let () = Printf.printf "/" in DIV }
  | '^'                 { let () = Printf.printf "^" in EXP }
  | '='                 { let () = Printf.printf "=" in EQ }
  | '>'                 { let () = Printf.printf ">" in GT }
  | '<'                 { let () = Printf.printf "<" in LT }
  | '['                 { let () = Printf.printf "[" in OSQUARE }
  | ']'                 { let () = Printf.printf "]" in CSQUARE }
  | '|'                 { let () = Printf.printf "|" in LISTSEP }
  | ">="                { let () = Printf.printf ">=" in GTE }
  | "<="                { let () = Printf.printf "<=" in LTE }
  | "cut"               { let () = Printf.printf "Cut\n" in CUT }
  | "fail"              { let () = Printf.printf "fail\n" in FAIL }
  | "."                 { let () = Printf.printf ".\n" in END }
  | ":-"                { let () = Printf.printf ":- " in SEP }
  | ","                 { let () = Printf.printf "," in COMMA }
  | "query"             { let () = Printf.printf "Query" in QUERY }
  | '('                 { let () = Printf.printf "(" in OPAREN }
  | ')'                 { let () = Printf.printf ")" in CPAREN }
  | ['A'-'Z']['a'-'z''A'-'Z''0'-'9']* as var  {let () = Printf.printf "Var " in Var(var)}
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as str  {let () = Printf.printf "Id " in Id(str)}
  | eol                 { token lexbuf }
  | eof                 { EOF }

