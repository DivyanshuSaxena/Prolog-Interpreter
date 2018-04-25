(* Sigma :- string * term list *)
exception TypeMismatch;; 
exception NotUnifiable;; 
open List;;
type term = Const of string | Var of string | FuncSym of string * (term list);;
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
					| (e,t)::tl -> if e=a then t else search tl a
					| [] -> raise TypeMismatch;;

let rec substitute s t = match t with
				| Const s -> Const s
				| Var a -> let ispresent e = (match e with (v,t) -> if v=a then true else false) in
						let orfunc a b = a || b in 
						(if (fold_left orfunc false (map ispresent s)) then (search s a) else (Var a))
				| FuncSym (sym,l) -> FuncSym (sym, map (substitute s) l);;

let rec subst sigma atom = match atom with
					| PredSym (name,terms) -> PredSym (name, map (substitute sigma) terms)
					| Cut -> Cut
					| Fail -> Fail;;

let rec subst_compose s1 s2 l = match s1 with
						| (v,t)::tl -> subst_compose tl s2 (l@[(v,substitute s2 t)]) 
						| [] -> let rec ispresent l e = (match l with 
														hd::tl -> if (fst hd)=(fst e) then [] else (ispresent tl e) | [] -> [e]) in
								let concat a b = a@b in 
								(l@(fold_left concat [] (map (ispresent l) s2)));;

let compose s1 s2 = subst_compose s1 s2 [];;

let rec fold_duo f g e l1 l2 = match (l1,l2) with
						| ((x1::xs1),(x2::xs2)) -> let sub = g x1 x2 in 
												(fold_duo f g (f e (g x1 x2)) (map (substitute sub) xs1) (map (substitute sub) xs2))
						| ([],[]) -> e
						| _ -> raise TypeMismatch;;

let rec vars term = match term with
					| Var a -> [a]
					| Const a -> []
					| FuncSym (s,l) -> fold_left_concat vars [] l;;

let rec mgu t1 t2 = match t1 with
					| Var a -> if t1=t2 then [] else (let rec search l e = (match l with hd::tl -> (if hd=e then true else false) | [] -> false) in
							if not (search (vars t2) a) then [(a,t2)] else raise NotUnifiable)
					| Const s -> if t1=t2 then [] else (mgu t2 t1)
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

let rec evaluate goals program subs = match goals with
					| [] -> subs
					| currgoal::gl ->  let result = []  in
						let rec goalloop prg goal goals = ( let rec matchgoal cl goal goals = (match cl with
							| Fact h -> let sigma = (unify h goal) in (if sigma=[] then [] else map (subst sigma) goals)
							| Rule (h,b) -> let sigma = (unify h goal) in (if sigma=[] then [] else (map (subst sigma) b@goals))
							| _ -> raise TypeMismatch ) in 
						(match prg with
							| cl::tl -> let sigma = (match cl with | Fact h -> (unify h goal) | Rule (h,b) -> (unify h goal) | _ -> raise TypeMismatch) in
										let remgoals = matchgoal cl goal goals in (match remgoals with
											| [] -> goalloop tl goal goals
											| _ -> result@(evaluate remgoals prg (compose sigma subs)) )
							| [] -> []) ) in (goalloop program currgoal gl);;

(* Test Cases *)
let goal = PredSym ("append", [FuncSym ("List",[Const "1"; Const "2"]); FuncSym ("List",[Const "3"; Const "4"]); Var "x"]);;
let prgm = [
	Fact (PredSym ("append", [FuncSym ("List", [Var "L"])])), 
	Rule (
		PredSym ("append", 
			[FuncSym("List", (Var "X")::(Var "Xs"));
			Var "L";
			FuncSym("List", (Var "X")::(Var "L2"))]
		),
		PredSym ("append",
			[Var "Xs"; Var "L"; Var "L2"]
		)
	)];;
evaluate goal prgm [];;
(* append([],L,L).
append([X|Xs],L,[X|L2]) :- append(Xs,L,L2). *)