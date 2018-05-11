exception TypeMismatch;; 
exception NotUnifiable;; 
exception GoalUnmatched;;
exception InvalidConstruct;;
exception EmptySubs;;
open List;;
open Unix;;
(* Sigma :- string * int * term list *) 
type symbol = List | Plus | Minus | Prod | Div | Mod | Exp | Eq | Gt | Lt | Gte | Lte;;
type term = Const of int | Bool of bool | Cons of string | Var of string * int | FuncSym of symbol * (term list);;
type atom = PredSym of string * (term list) | Cut | Fail;;
type clause = Fact of atom | Rule of (atom * (atom list)) | Query of (atom list);;
type answer = Map of (string * term) list | Claim of bool;;
type program = clause list;;

let rec print_term term = let rec print_tlist tlist = (match tlist with
    | hd::tl -> (print_term hd)^"; "^(print_tlist tl)
    | [] -> "]") in (match term with
		    | Const n -> string_of_int n
		    | Cons s -> s
		    | Bool b -> string_of_bool b
		    | Var (v,n) -> v
		    | FuncSym (List,tlist) -> "List["^(print_tlist tlist)
		    | FuncSym (Plus,tlist) -> "Plus["^(print_tlist tlist)
		    | FuncSym (Minus,tlist) -> "Minus["^(print_tlist tlist)
		    | FuncSym (Prod,tlist) -> "Prod["^(print_tlist tlist)
		    | FuncSym (Div,tlist) -> "Div["^(print_tlist tlist)
		    | FuncSym (Mod,tlist) -> "Mod["^(print_tlist tlist)
		    | FuncSym (Exp,tlist) -> "Exp["^(print_tlist tlist)
		    | FuncSym (Eq,tlist) -> "Eq["^(print_tlist tlist)
		    | FuncSym (Gt,tlist) -> "Gt["^(print_tlist tlist)
		    | FuncSym (Lt,tlist) -> "Lt["^(print_tlist tlist)
		    | FuncSym (Gte,tlist) -> "Gte["^(print_tlist tlist)
		    | FuncSym (Lte,tlist) -> "Lte["^(print_tlist tlist) );;

let print_atom atom = (match atom with
    | PredSym(str,tlist) -> let rec print_tlist tlist = (match tlist with
							    | hd::tl -> (print_term hd)^"; "^(print_tlist tl)
							    | [] -> "]") in str^"["^(print_tlist tlist)
    | Cut -> "Cut" | Fail -> "Fail");;

let rec print_alist alist = (match alist with
	| hd::tl -> (print_atom hd)^"; "^(print_alist tl)
	| [] -> "]");;

let print_clause clause = (match clause with
    | Fact atom -> (print_atom atom)^"."
    | Rule (atom,alist) -> (print_atom atom)^":- ["^(print_alist alist)^"."
    | _ -> raise InvalidConstruct);;

let rec print_program prg = (match prg with
    | hd::tl -> (print_clause hd)^"; "^(print_program tl)
    | [] -> "]");;

let rec print_answer ans = match ans with
	| Map l -> let rec print_maplist mapl = (match mapl with
				| (v,t)::tl -> " ("^v^","^(print_term t)^(print_maplist tl)^") "
				| [] -> "]") in "["^(print_maplist l)
	| Claim true -> "true"
	| Claim false -> "false";;

let rec print_anslist al = String.concat ";" (map print_answer al);;

let rec print_subs v = match v with (f,s,t) -> "("^f^","^(string_of_int s)^","^(print_term t)^")";;

let rec print_subslist subslist = String.concat ";" (List.map print_subs subslist);;

let rec fold_left f e l = match l with	
					| [] -> e
					| hd::tl -> fold_left f (f hd e) tl;;

let rec fold_left_concat f e l = match l with
					| [] -> e
					| hd::tl -> fold_left_concat f (e@(f hd)) tl;;

let rec search substlist a = match substlist with
					| (e,n,t)::tl -> if (e,n)=a then t else search tl a
					| [] -> raise TypeMismatch;;

let rec substitute s t = match t with
				| Const s -> Const s
				| Cons s -> Cons s
				| Bool b -> Bool b
				| Var (a,n) -> let ispresent e = (match e with (v,m,t) -> if (v,m)=(a,n) then true else false) in
						let orfunc a b = a || b in 
						(if (fold_left orfunc false (map ispresent s)) then (search s (a,n)) else (Var (a,n)))
				| FuncSym (sym,l) -> FuncSym (sym, map (substitute s) l);;

let rec subst sigma atom = match atom with
					| PredSym (name,terms) -> PredSym (name, map (substitute sigma) terms)
					| Cut -> Cut
					| Fail -> Fail;;

let substsubst sigma e = match e with
					| (v,n,t) -> (v,n,substitute sigma t);;

let rec subst_compose s1 s2 l = match s1 with
						| (v,n,t)::tl -> subst_compose tl s2 (l@[(v,n,substitute s2 t)]) 
						| [] -> let rec ispresent l e = (match l with 
														| (hdfst,hdsnd,hdt)::tl -> (match e with (v,n,t) -> if (hdfst=v && hdsnd=n) then [] else (ispresent tl e)) 
														| [] -> [e]) in
								let concat a b = b@a in 
								(l@(fold_left concat [] (map (ispresent l) s2)));;

let compose s1 s2 = subst_compose s1 s2 [];;

let rec fold_duo f g e l1 l2 = match (l1,l2) with
						| ((x1::xs1),(x2::xs2)) -> let sub = g x1 x2 in 
												(fold_duo f g (f e (g x1 x2)) (map (substitute sub) xs1) (map (substitute sub) xs2))
						| ([],[]) -> e
						| _ -> raise TypeMismatch;;

let rec vars term = match term with
					| Var (a,n) -> [(a,n)]
					| Const a -> []
					| Cons s -> []
					| Bool b -> []
					| FuncSym (s,l) -> fold_left_concat vars [] l;;

let rec mgu t1 t2 = match t1 with
					| Var (a,n) -> if t1=t2 then [] else [(a,n,t2)]
					| Const s -> if t1=t2 then [] else (match t2 with | Const s2 -> raise NotUnifiable | _ -> (mgu t2 t1))
					| Cons s -> if t1=t2 then [] else (match t2 with | Cons s2 -> raise NotUnifiable | _ -> (mgu t2 t1))
					| Bool b -> if t1=t2 then [] else (match t2 with | Bool b2 -> raise NotUnifiable | _ -> (mgu t2 t1))
					| FuncSym (s, l) -> (match s with 
						| List -> (match t2 with
								| Var (a,n) -> mgu t2 t1 | Const n2 -> raise NotUnifiable | Bool b2 -> raise NotUnifiable | Cons s2 -> raise NotUnifiable
								| FuncSym (List, []) -> if l=[] then [] else raise NotUnifiable
								| FuncSym (List, l2) -> let rec mgulist l1 l2 = (match (l1,l2) with
										| (x1::xs1,x2::xs2) -> let sigma = mgu x1 x2 in compose sigma (mgulist (map (substitute sigma) xs1) (map (substitute sigma) xs2))
										| ([],xs) -> let isvar e = (match e with | (Var (a,n)) -> true | _ -> false) in 
													let mapempty e = (match e with | (Var (a,n)) -> (a,n,FuncSym (List, [])) | _ -> raise NotUnifiable) in
													let andf a b = a && b in
													(if (fold_left andf true (map isvar xs)) then (map mapempty xs) else raise NotUnifiable) 
										| (xs,[]) -> mgulist l2 l1 ) 
									in (mgulist l l2)
								| FuncSym (_, l2) -> raise NotUnifiable ) 
						| _ -> (match t2 with
								| Var (a,n) -> mgu t2 t1 | Const s2 -> raise NotUnifiable | Bool b2 -> raise NotUnifiable | Cons c2 -> raise NotUnifiable
								| FuncSym (s2,l2) -> if (s=s2 && (length l)=(length l2)) then (fold_duo compose mgu [] l l2) 
									else raise NotUnifiable) );;

let rec unify atom1 atom2 = match (atom1,atom2) with
					| (PredSym (s1,terml1),PredSym (s2,terml2)) -> if s1=s2 then (try
						let subs = fold_duo compose mgu [] terml1 terml2 in
						if subs=[] then raise EmptySubs else subs
						with 
						| EmptySubs -> raise EmptySubs
						| TypeMismatch -> []
						| NotUnifiable -> []) else []
					| _ -> [];; 

let rec matchgoal cl goal goals = (match cl with
						| Fact h -> (try 
							let sigma = (unify h goal) in (if sigma=[] then [] else map (subst sigma) goals) 
							with
							| EmptySubs -> goals)
						| Rule (h,b) -> (try 
							let sigma = (unify h goal) in (if sigma=[] then [] else (map (subst sigma) b@goals))
							with
							| EmptySubs -> goals)
						| _ -> raise TypeMismatch );;

let rec increvar term = match term with
						| Var (s,n) -> Var (s,n+1)
						| Const n -> Const n
						| Cons s -> Cons s
						| Bool b -> Bool b
						| FuncSym (s,tl) -> FuncSym (s,map increvar tl);;

let incratom atom  = match atom with
						| PredSym (s,terml) -> PredSym (s, map increvar terml)
						| Cut -> Cut
						| Fail -> Fail;;

let rec increment e = (match e with (v,n,t) -> (v,n+1,increvar t));;

let getfst v = match v with (f,s,t) -> f;;
let getsnd v = match v with (f,s,t) -> s;;
let gethird v = match v with (f,s,t) -> t;;

let rec evalquery goals program rem subs stack = let subsnamed = (map increment subs) in 
								Printf.printf "Rem prg :-  %s\n" (print_program rem);
								match goals with
								| [] -> [subsnamed]
								| Cut::nextgoals -> evalquery nextgoals program rem subsnamed []
								| Fail:: nextgoals -> raise GoalUnmatched
								| currgoal::nextgoals -> let goals = currgoal::nextgoals in (match rem with
									| cl::tl -> let sigma = (match cl with 
											| Fact h -> (try (unify h (hd goals)) with | EmptySubs -> [("undefined",-1,Const (-999))]) 
											| Rule (h,b) -> (try (unify h (hd goals)) with | EmptySubs -> [("undefined",-1,Const (-999))]) 
											| _ -> raise TypeMismatch) in
											(match sigma with
												| [] -> Printf.printf "Goal did not match and stack empty\n"; evalquery goals program tl subs stack
												| _ -> let remgoals = matchgoal cl (hd goals) (List.tl goals) in 
														Printf.printf "%s\n" (print_alist remgoals); (match remgoals with
														| [] -> if stack=[] then ( Printf.printf "Goals and stack empty\n"; [(compose sigma subsnamed)] ) else 
															( Printf.printf "No more goals but stack remaining\n"; [(compose sigma subsnamed)]@(evalquery (getfst (List.hd stack)) program (getsnd (List.hd stack)) [] (List.tl stack)) )
														| _ -> ( Printf.printf "Goals remaining\n";  (evalquery (map incratom (map (subst sigma) remgoals)) program program (compose sigma subsnamed) ([goals,tl,subsnamed]@stack))) ))
									| [] -> if stack=[] then ( Printf.printf "Program and stack empty\n";  []) else ((* Printf.printf "Program empty, stack remaining\n"; *) evalquery (getfst (hd stack)) program (getsnd (hd stack)) (gethird (hd stack)) (tl stack)) );;

let rec evaluate program querylist = let stripquery queries = (match queries with | (Query l) -> l | _ -> raise TypeMismatch) in
						let goals = stripquery querylist in
						(try
							(* let rawsubslist = evalquery goals program [] [] in *)
							let rawsubslist = evalquery goals program program [] [(goals,List.tl program,[])] in
							Printf.printf "%s\n" (String.concat "|" (List.map print_subslist rawsubslist));
							let rec reduce subs ans = (match subs with
										| (v,n,t)::tl -> if tl=[] then (if not (String.contains v '#') then ans else ((v,t)::ans)) else
												(if (String.contains v '#') then reduce tl ((v,t)::ans) else reduce (map (substsubst [(v,n,t)]) tl) ans)
										| [] -> []) in
							let convans finalsubs = (match finalsubs with
								| [] -> Claim (true)
								| hd::tl -> Map (hd::tl)) in
							let rec reducelist rawsubs = (match rawsubs with
								| hd::tl -> (reduce hd [])::(reducelist tl)
								| [] -> []) in
							match rawsubslist with
							| [] -> raise GoalUnmatched
							| _ -> (map convans (reducelist rawsubslist))
						with
						| GoalUnmatched -> [Claim false]);;


(* Test Cases *)
(* Test Case 1 *)

(* let goal1 = PredSym ("append", [FuncSym (List, [Const 1; FuncSym (List, [Const 2])]); FuncSym (List, [Const 3; FuncSym (List, [Const 4; Const 5])]); Var ("#x",0)]);;
let goal2 = PredSym ("append", [FuncSym (List, [Const 1]); FuncSym (List, [Const 3]); FuncSym(List, [Const 1; FuncSym (List, [Const 4])])]);;
let goal3 = PredSym ("append", [FuncSym (List, []); FuncSym (List, [Const 3]); FuncSym(List, [Const 1])]);;
let facth = PredSym ("append", [FuncSym (List, []); Var ("L",0); Var ("L",0)]);;
let ruleh = PredSym ("append", [FuncSym (List, [Var ("X",0); Var ("Xs",0)]); Var ("L",0); FuncSym (List, [Var ("X",0); Var ("L2",0)]) ]);;
let rule = 	Rule (
		ruleh,
		[PredSym ("append",
			[Var ("Xs",0); Var ("L",0); Var ("L2",0)]
		)]
	);;
let prgm = [
	Fact (
		PredSym ("append", 
			[FuncSym (List, []); Var ("L",0); Var ("L",0)]
		)
	);
	Rule (
		ruleh,
		[PredSym ("append", 
			[Var ("Xs",0); Var ("L",0); Var ("L2",0)]
		)]
	)];;
evalquery [goal2] prgm [] [];;
evalquery [goal1] prgm [] [];;
evaluate prgm (Query [goal1]);;
evaluate prgm (Query [goal2]);; *)

(* append([],L,L).
append([X|Xs],L,[X|L2]) :- append(Xs,L,L2). *)

(* Test Case 2 *)
let memfact = Fact(PredSym("member", [Var ("X",0); FuncSym (List, [Var ("X",0); Var ("Xs",0)])]));;
let memrule = Rule(PredSym("member", [Var ("X",0); FuncSym (List, [Var ("Y",0); Var ("L",0)])]), [PredSym("member", [Var("X",0); Var("L",0)])]);;
let p = [memfact;memrule];;
let goal4 = PredSym ("member",[Var ("#Z",0); FuncSym (List, [Const 1; FuncSym (List, [Const 2])])]);;
let goals5 = [PredSym ("member",[Var ("#Z",0); FuncSym (List, [Const 1; FuncSym (List, [Const 2])])]); PredSym ("member",[Var("#Z",0); FuncSym (List, [Const 2; FuncSym (List, [Const 3])])])];;
let q1 = Query(
			[PredSym ("member",
				[Var ("#Z",0); 
				FuncSym (List, [Const 1; FuncSym (List, [Const 2])])
				]
			); 
			PredSym ("member",
				[Var("#Z",0); 
				FuncSym (List, [Const 2; FuncSym (List, [Const 3])])
				]
			)]
		);;
let q2 = Query([PredSym ("member",[Var ("#Z",0); FuncSym (List, [Const 1; FuncSym (List, [Const 2])])])]);;
(* evalquery [goal4] p [] [];; *)
(* evalquery goals5 p [] [];; *)
(* evaluate p q2;; *)
evaluate p q1;;

(* member(X,[X|Y]).
member(X,[Y|L]) :- member(X,L). *)						