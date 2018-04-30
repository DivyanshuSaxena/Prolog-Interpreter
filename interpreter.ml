exception TypeMismatch;; 
exception NotUnifiable;; 
exception GoalUnmatched;;
open List;;
(* Sigma :- string * int * term list *) 
(* Increment in substitution woring fine but unification with fact for bigger lists failing *)
type symbol = List;;
type term = Const of string | Var of string * int | FuncSym of symbol * (term list);;
type atom = PredSym of string * (term list) | Cut | Fail;;
type clause = Fact of atom | Rule of (atom * (atom list)) | Query of (atom list);;
type answer = Map of (string * term) list | Claim of bool;;

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
					| FuncSym (s,l) -> fold_left_concat vars [] l;;

let rec mgu t1 t2 = match t1 with
					| Var (a,n) -> if t1=t2 then [] else [(a,n,t2)]
					| Const s -> if t1=t2 then [] else (match t2 with | Const s2 -> raise NotUnifiable | _ -> (mgu t2 t1))
					| FuncSym (List, l) -> (match t2 with
								| Var (a,n) -> mgu t2 t1 | Const s2 -> raise NotUnifiable
								| FuncSym (List, []) -> if l=[] then [] else raise NotUnifiable
								| FuncSym (List, l2) -> let rec mgulist l1 l2 = (match (l1,l2) with
										| (x1::xs1,x2::xs2) -> let sigma = mgu x1 x2 in compose sigma (mgulist (map (substitute sigma) xs1) (map (substitute sigma) xs2))
										| ([],xs) -> let isvar e = (match e with | (Var (a,n)) -> true | _ -> false) in 
													let mapempty e = (match e with | (Var (a,n)) -> (a,n,FuncSym (List, [])) | _ -> raise NotUnifiable) in
													let andf a b = a && b in
													(if (fold_left andf true (map isvar xs)) then (map mapempty xs) else raise NotUnifiable) 
										| (xs,[]) -> mgulist l2 l1 ) 
									in (mgulist l l2) );;
								| _ -> raise NotUnifiable);;
					| FuncSym (s,l) -> (match t2 with
								| Var a -> mgu t2 t1 | Const s2 -> raise NotUnifiable
								| FuncSym (s2,l2) -> if (s=s2 && (length l)=(length l2)) then (fold_duo compose mgu [] l l2) 
									else raise NotUnifiable);;

let rec unify atom1 atom2 = match (atom1,atom2) with
					| (PredSym (s1,terml1),PredSym (s2,terml2)) -> (try
						fold_duo compose mgu [] terml1 terml2
						with 
						| TypeMismatch -> []
						| NotUnifiable -> [])
					| _ -> [];; 

let rec matchgoal cl goal goals = (match cl with
						| Fact h -> let sigma = (unify h goal) in (if sigma=[] then [] else map (subst sigma) goals)
						| Rule (h,b) -> let sigma = (unify h goal) in (if sigma=[] then [] else (map (subst sigma) b@goals))
						| _ -> raise TypeMismatch );;

let rec increvar term = match term with
						| Var (s,n) -> Var (s,n+1)
						| Const n -> Const n
						| FuncSym (s,tl) -> FuncSym (s,map increvar tl);;

let incratom atom  = match atom with
						| PredSym (s,terml) -> PredSym (s, map increvar terml)
						| Cut -> Cut
						| Fail -> Fail;;

let rec increment e = (match e with (v,n,t) -> (v,n+1,increvar t));;

let rec evalquery goals program subs stack = let subsnamed = (map increment subs) in
						match goals with
						| [] -> [subsnamed]
						| currgoal::gl ->  let rec goalloop prg rem goal goals stack = (match rem with
								| cl::tl -> let sigma = (match cl with | Fact h -> (unify h goal) | Rule (h,b) -> (unify h goal) | _ -> raise TypeMismatch) in
										(match sigma with
											| [] -> goalloop prg tl goal goals stack
											| _ -> let remgoals = matchgoal cl goal goals in (match remgoals with
													| [] -> if stack=[] then [(compose sigma subsnamed)] else (goalloop prg tl goal goals (List.tl stack))@[(compose sigma subsnamed)]
													| _ -> (evalquery (map incratom remgoals) prg (compose sigma subsnamed) ([goal::goals,tl]@stack)) ))
								| [] -> if stack=[] then [] else 
											goalloop prg (snd (hd stack)) (hd (fst (hd stack))) (tl (fst (hd stack))) (tl stack) ) in 
										(goalloop program program currgoal gl stack);;

let rec evaluate program queries = let stripquery queries = (match queries with | (Query l) -> l | _ -> raise TypeMismatch) in
						let goals = stripquery queries in
						(try
							let rawsubslist = evalquery goals program [] [] in
							let rec reduce ans subs = (match subs with
										| (v,n,t)::tl -> if tl=[] then (if not (String.contains v '#') then ans else ((v,t)::ans)) else
												(if (String.contains v '#') then reduce ((v,t)::ans) tl else reduce ans (map (substsubst [(v,n,t)]) tl))
										| [] -> []) in
							let convans finalsubs = (match finalsubs with
								| [] -> Claim (true)
								| hd::tl -> Map (hd::tl)) in
							let partlist = (match rawsubslist with | [] -> raise GoalUnmatched | _ -> map (reduce []) rawsubslist) in
							(map convans partlist)
						with
						| GoalUnmatched -> [Claim (false)]
						| _ -> failwith "Unknown");;
						
(* Test Cases *)
(* Test Case 1 *)
let goal1 = PredSym ("append", [FuncSym (List, [Const "1"; FuncSym (List, [Const "2"])]); FuncSym (List, [Const "3"; FuncSym (List, [Const "4"; Const "5"])]); Var ("#x",0)]);;
let goal2 = PredSym ("append", [FuncSym (List, [Const "1"]); FuncSym (List, [Const "3"]); FuncSym(List, [Const "1"; FuncSym (List, [Const "4"])])]);;
let goal3 = PredSym ("append", [FuncSym (List, []); FuncSym (List, [Const "3"]); FuncSym(List, [Const "1"])]);;
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
evaluate prgm (Query [goal2]);;

(* append([],L,L).
append([X|Xs],L,[X|L2]) :- append(Xs,L,L2). *)

(* Test Case 2 *)
let memfact = Fact(PredSym("member", [Var ("X",0); FuncSym (List, [Var ("X",0); Var ("Y",0)])]));;
let memrule = Rule(PredSym("member", [Var ("X",0); FuncSym (List, [Var ("Y",0); Var ("L",0)])]), [PredSym("member", [Var("X",0); Var("L",0)])]);;
let p = [memfact;memrule];;
let goal4 = PredSym ("member",[Var ("#Z",0); FuncSym (List, [Const "a"; FuncSym (List, [Const "b"])])]);;
let goals5 = [PredSym ("member",[Var ("#Z",0); FuncSym (List, [Const "a"; Const "b"])]); PredSym ("member",[Var("#Z",0); FuncSym (List, [Const "b";Const "c"])])];;
let q1 = Query([PredSym ("member",[Var ("#Z",0); FuncSym (List, [Const "a"; Const "b"])]); PredSym ("member",[Var("#Z",0); FuncSym (List, [Const "b";Const "c"])])]);;
let q2 = Query([PredSym ("member",[Var ("#Z",0); FuncSym (List, [Const "a"; Const "b"])])]);;
(* evalquery goals5 p [] [];; *)
evaluate p q2;;
evaluate p q1;;

(* member(X,[X|Y]).
member(X,[Y|L]) :- member(X,L). *)