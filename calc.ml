exception InvalidConstruct;;
open Interpreter;;
let _ =
	  try
	    let lexbuf = Lexing.from_channel stdin in
	    let rec print_term term = let rec print_tlist tlist = (match tlist with
									    | hd::tl -> (print_term hd)^"; "^(print_tlist tl)
									    | [] -> "]") in (match term with
		    | Const n -> string_of_int n
		    | Cons s -> s
		    | Bool b -> string_of_bool b
		    | Var (v,n) -> v
		    | FuncSym (List,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Plus,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Minus,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Prod,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Div,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Mod,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Exp,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Eq,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Gt,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Lt,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Gte,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Lte,tlist) -> "List["^(print_tlist tlist) ) in
	    let print_atom atom = (match atom with
		    | PredSym(str,tlist) -> let rec print_tlist tlist = (match tlist with
									    | hd::tl -> (print_term hd)^"; "^(print_tlist tl)
									    | [] -> "]") in str^"["^(print_tlist tlist)
		    | Cut -> "Cut" | Fail -> "Fail") in
	    let rec print_alist alist = (match alist with
	    	| hd::tl -> (print_atom hd)^"; "^(print_alist tl)
	    	| [] -> "]") in
	    let print_clause clause = (match clause with
		    | Fact atom -> (print_atom atom)^"."
		    | Rule (atom,alist) -> (print_atom atom)^":- ["^(print_alist alist)^"."
		    | _ -> raise InvalidConstruct) in
	    let rec print_program prg = (match prg with
		    | hd::tl -> (print_clause hd)^"; "^(print_program tl)
		    | [] -> "]") in
	    while true do
	      let result = Parser.main Lexer.token lexbuf in
	        (print_program result); (print_newline()); flush stdout
	    done
	  with Lexer.Eof ->
	    exit 0