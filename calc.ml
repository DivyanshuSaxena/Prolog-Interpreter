open Parser;;
open Interpreter;;
open Printf;;

let rec hashvar term = match term with
						| Var (s,n) -> Var ("#"^s,n)
						| Const n -> Const n
						| Cons s -> Cons s
						| Bool b -> Bool b
						| FuncSym (s,tl) -> FuncSym (s,List.map increvar tl);;

let hashatom atom  = match atom with
						| PredSym (s,terml) -> PredSym (s, List.map hashvar terml)
						| Cut -> Cut
						| Fail -> Fail;;

let rec hashprg program = match program with
					| hd::tl -> (match hd with
							| Fact h -> (Fact (hashatom h))::(hashprg tl)
							| Rule (h,b) -> (Rule (hashatom h, List.map hashatom b))::(hashprg tl) )
					| [] -> [];;

let load_rule fl = 
  let decl = open_in fl in
  let lexbuf = Lexing.from_channel decl in
  let prog = Parser.main Lexer.token lexbuf in
  (* Printf.printf "%s\n" (print_program prog); *) prog;;

let handle_query prog str = 
  let lexbuf = Lexing.from_string str in
  let goals = Parser.goal Lexer.token lexbuf in
  let stripquery = (match goals with Query l -> l) in
  let hashgoals = List.map hashatom stripquery in 
  (* Printf.printf "%s\n" (String.concat ";" (List.map print_atom hashgoals)); *) let ans = Interpreter.evaluate prog (Query hashgoals) in 
  Interpreter.print_anslist ans;;

let rec main prog =
    Printf.printf "?- ";
    let inp = try read_line() with End_of_file -> Printf.printf "\nExiting\n" ;exit 0 in
    let () = flush stdout in            
    let len = String.length inp in
    if (inp.[(len-1)] <> '.') then
      let () = (Printf.printf "Every instruction must end with \'.\'\n") in
      main prog
    else
      if (inp = "halt.") then exit 0
      else
        if ((String.sub inp 0 2) = "[\"") && ((String.sub inp (len - 3) 3) = "\"].") then
          let name = String.sub inp 2 (len - 5) in 
          let new_prog = try let prog = (load_rule name) in let () = Printf.printf "Program loaded.\n" in prog with
            | Parsing.Parse_error -> let () = Printf.printf("Error in rule file given.\n") in prog 
            | _ -> let () = Printf.printf("Wrong file path provided.\n") in prog
          in main new_prog
        else
          let answer = try (handle_query prog inp)
          with
          | Parsing.Parse_error -> Printf.printf "Invalid command\n"; "Retry."
          in (Printf.printf "%s\n" answer); main prog;;


if !Sys.interactive then () else main [];;
    


(* exception InvalidConstruct;;
open Parser;;
(* open Interpreter;; *)
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
		    | hd::[] -> (print_clause hd)
		    | hd::tl -> (print_clause hd)^"; "^(print_program tl)
		    | [] -> "]") in
	    while true do
	      let result = Parser.main Lexer.token lexbuf in
	      let l = evaluate result (Query []) in
	      print_string (print_program result); (print_newline()); flush stdout
	    done
	  with Lexer.Eof ->
	    exit 0 *)