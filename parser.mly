%{
open Interpreter
open Printf
%}

%token <string> Var Id 
%token <int> Const
%token <bool> Bool
%token COMMA SEP OPAREN CPAREN
%token PLUS MINUS PROD DIV MOD EXP EQ GT LT GTE LTE AND OR NOT
%token LISTSEP OSQUARE CSQUARE
%token CUT FAIL
%token QUERY SEP END
%token EOF

%start main
%start goal
%type <Interpreter.program> main
%type <Interpreter.clause> goal

%%

main: EOF							{ [] }
	| clauselist main				{ ($1)@($2) }

goal: atomlist						{ Query($1) }

clauselist: clause COMMA clauselist	{ ($1)::($3) }
	| clause 						{ [$1] }

clause: QUERY atomlist END			{ Query($2) }
	| atom END						{ Fact($1) }
	| atom SEP atomlist END			{ Rule($1,$3) }

atomlist: atom COMMA atomlist		{ ($1)::($3) }
	| atom 							{ [$1] }

atom: Id OPAREN termlist CPAREN		{ PredSym($1,$3) }
	| CUT 							{ Cut }
	| FAIL 							{ Fail }

termlist: term LISTSEP term			{ ($1)::[$3] } 
	| term COMMA termlist			{ ($1)::($3) }
	| term 							{ [$1] }

term: Const 						{ Const($1) }
	| Bool 							{ Bool($1) }
	| Var 							{ Var($1,0) }
	| OSQUARE CSQUARE		 		{ FuncSym(List,[]) }
	| OSQUARE termlist CSQUARE 		{ FuncSym(List,$2) }
	| symbol termlist				{ FuncSym($1,$2) }
	| Id							{ Cons($1) }

symbol:	PLUS						{ Plus }
	| MINUS 						{ Minus }
	| PROD 							{ Prod }
	| DIV 							{ Div }
	| MOD 							{ Mod }
	| EXP							{ Exp } 
	| EQ							{ Eq } 
	| GT							{ Gt } 
	| LT							{ Lt } 
	| GTE							{ Gte } 
	| LTE							{ Lte } 