{
open Parser        (* The type token is defined in parser.mly *)
open Printf
exception Eof
}
let eol = '\n'

rule token = parse
    [' ' '\t']          { token lexbuf } 
  | ['0'-'9']+ as num   { let () = Printf.printf "Const\n" in Const(int_of_string num) }
  | "true"              { let () = Printf.printf "True\n" in Bool(true) }
  | "false"             { let () = Printf.printf "False\n" in Bool(false) }
  | '+'                 { let () = Printf.printf "+\n" in PLUS }
  | '-'                 { let () = Printf.printf "-\n" in MINUS }
  | '*'                 { let () = Printf.printf "*\n" in PROD }
  | '/'                 { let () = Printf.printf "/\n" in DIV }
  | '^'                 { let () = Printf.printf "^\n" in EXP }
  | '='                 { let () = Printf.printf "=\n" in EQ }
  | '>'                 { let () = Printf.printf ">\n" in GT }
  | '<'                 { let () = Printf.printf "<\n" in LT }
  | '['                 { let () = Printf.printf "[\n" in OSQUARE }
  | ']'                 { let () = Printf.printf "]\n" in CSQUARE }
  | '|'                 { let () = Printf.printf "|\n" in LISTSEP }
  | ">="                { let () = Printf.printf ">=\n" in GTE }
  | "<="                { let () = Printf.printf "<=\n" in LTE }
  | "cut"               { let () = Printf.printf "Cut\n" in CUT }
  | "fail"              { let () = Printf.printf "fail\n" in FAIL }
  | "."                 { let () = Printf.printf ".\n" in END }
  | ":-"                { let () = Printf.printf ":- \n" in SEP }
  | ","                 { let () = Printf.printf ",\n" in COMMA }
  | "query"             { let () = Printf.printf "Query\n" in QUERY }
  | '('                 { let () = Printf.printf "(\n" in OPAREN }
  | ')'                 { let () = Printf.printf ")\n" in CPAREN }
  | ['A'-'Z']['a'-'z''A'-'Z''0'-'9']* as var  {let () = Printf.printf "Var \n" in Var(var)}
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as str  {let () = Printf.printf "Id \n" in Id(str)}
  | eol                 { token lexbuf }
  | eof                 { EOF }

