type symbol = List;;
type term = Const of string 
			| Var of string * int 
			| FuncSym of symbol * (term list);;
type atom = PredSym of string * (term list) 
			| Cut 
			| Fail;;
type clause = Fact of atom 
			| Rule of (atom * (atom list)) 
			| Query of (atom list);;