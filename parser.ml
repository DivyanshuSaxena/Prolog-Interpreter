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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Interpreter
# 38 "parser.ml"
let yytransl_const = [|
  261 (* COMMA *);
  262 (* SEP *);
  263 (* OPAREN *);
  264 (* CPAREN *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* PROD *);
  268 (* DIV *);
  269 (* MOD *);
  270 (* EXP *);
  271 (* EQ *);
  272 (* GT *);
  273 (* LT *);
  274 (* GTE *);
  275 (* LTE *);
  276 (* AND *);
  277 (* OR *);
  278 (* NOT *);
  279 (* LISTSEP *);
  280 (* OSQUARE *);
  281 (* CSQUARE *);
  282 (* CUT *);
  283 (* FAIL *);
  284 (* QUERY *);
  285 (* END *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* Var *);
  258 (* Id *);
  259 (* Const *);
  260 (* Bool *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\004\000\
\004\000\005\000\005\000\005\000\006\000\006\000\006\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\000\000"

let yylen = "\002\000\
\001\000\002\000\003\000\001\000\003\000\002\000\004\000\003\000\
\001\000\004\000\001\000\001\000\003\000\003\000\001\000\001\000\
\001\000\001\000\002\000\003\000\002\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\011\000\012\000\000\000\001\000\034\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\
\000\000\006\000\018\000\022\000\016\000\017\000\023\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\000\000\000\000\000\000\000\000\005\000\000\000\003\000\
\000\000\019\000\000\000\010\000\000\000\000\000\021\000\008\000\
\007\000\020\000\014\000\013\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\013\000\011\000\035\000\036\000\037\000"

let yysindex = "\008\000\
\001\000\000\000\005\255\000\000\000\000\003\255\000\000\000\000\
\001\000\008\255\002\255\052\255\241\254\011\255\000\000\000\255\
\003\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\255\009\255\001\255\052\255\000\000\003\255\000\000\
\246\254\000\000\251\254\000\000\052\255\052\255\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\000\000\000\000\000\000\248\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\013\000\007\000\000\000\242\255\250\255\229\255\248\255\000\000"

let yytablesize = 288
let yytable = "\014\000\
\007\000\003\000\041\000\004\000\003\000\045\000\043\000\017\000\
\001\000\047\000\014\000\012\000\016\000\038\000\015\000\039\000\
\044\000\051\000\049\000\050\000\009\000\015\000\040\000\046\000\
\048\000\004\000\005\000\006\000\004\000\005\000\018\000\015\000\
\014\000\019\000\020\000\021\000\022\000\052\000\000\000\000\000\
\000\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\019\000\020\000\021\000\022\000\
\034\000\042\000\000\000\000\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\000\000\
\000\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\000\000\000\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\005\000\006\000\004\000\004\000\004\000"

let yycheck = "\006\000\
\000\000\002\001\017\000\000\000\002\001\005\001\034\000\006\001\
\001\000\037\000\017\000\007\001\005\001\029\001\008\001\005\001\
\008\001\045\000\029\001\025\001\029\001\009\000\016\000\023\001\
\039\000\026\001\027\001\028\001\026\001\027\001\029\001\025\001\
\039\000\001\001\002\001\003\001\004\001\046\000\255\255\255\255\
\255\255\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\001\001\002\001\003\001\004\001\
\024\001\025\001\255\255\255\255\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\255\255\
\255\255\255\255\255\255\024\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\002\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\026\001\027\001\028\001\026\001\027\001\028\001"

let yynames_const = "\
  COMMA\000\
  SEP\000\
  OPAREN\000\
  CPAREN\000\
  PLUS\000\
  MINUS\000\
  PROD\000\
  DIV\000\
  MOD\000\
  EXP\000\
  EQ\000\
  GT\000\
  LT\000\
  GTE\000\
  LTE\000\
  AND\000\
  OR\000\
  NOT\000\
  LISTSEP\000\
  OSQUARE\000\
  CSQUARE\000\
  CUT\000\
  FAIL\000\
  QUERY\000\
  END\000\
  EOF\000\
  "

let yynames_block = "\
  Var\000\
  Id\000\
  Const\000\
  Bool\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 20 "parser.mly"
                ( [] )
# 241 "parser.ml"
               : Interpreter.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clauselist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Interpreter.program) in
    Obj.repr(
# 21 "parser.mly"
                      ( (_1)@(_2) )
# 249 "parser.ml"
               : Interpreter.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'clauselist) in
    Obj.repr(
# 23 "parser.mly"
                                    ( (_1)::(_3) )
# 257 "parser.ml"
               : 'clauselist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 24 "parser.mly"
                ( [_1] )
# 264 "parser.ml"
               : 'clauselist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomlist) in
    Obj.repr(
# 26 "parser.mly"
                             ( let () = Printf.printf "Query\n" in Query(_2) )
# 271 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 27 "parser.mly"
                 ( Fact(_1) )
# 278 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'atomlist) in
    Obj.repr(
# 28 "parser.mly"
                           ( Rule(_1,_3) )
# 286 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomlist) in
    Obj.repr(
# 30 "parser.mly"
                               ( (_1)::(_3) )
# 294 "parser.ml"
               : 'atomlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 31 "parser.mly"
               ( let () = Printf.printf "Single atom\n" in [_1] )
# 301 "parser.ml"
               : 'atomlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'termlist) in
    Obj.repr(
# 33 "parser.mly"
                                 ( let () = Printf.printf "PredSym\n" in PredSym(_1,_3) )
# 309 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
              ( Cut )
# 315 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
               ( Fail )
# 321 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 37 "parser.mly"
                              ( (_1)::[_3] )
# 329 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'termlist) in
    Obj.repr(
# 38 "parser.mly"
                         ( (_1)::(_3) )
# 337 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 39 "parser.mly"
               ( [_1] )
# 344 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 41 "parser.mly"
                  ( Const(_1) )
# 351 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 42 "parser.mly"
               ( Bool(_1) )
# 358 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "parser.mly"
              ( Var(_1,0) )
# 365 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                       ( FuncSym(List,[]) )
# 371 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'termlist) in
    Obj.repr(
# 45 "parser.mly"
                              ( FuncSym(List,_2) )
# 378 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'symbol) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'termlist) in
    Obj.repr(
# 46 "parser.mly"
                      ( FuncSym(_1,_2) )
# 386 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 47 "parser.mly"
            ( let () = Printf.printf "Found Id\n" in Cons(_1) )
# 393 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                  ( Plus )
# 399 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
               ( Minus )
# 405 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
               ( Prod )
# 411 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
              ( Div )
# 417 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
              ( Mod )
# 423 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
             ( Exp )
# 429 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
            ( Eq )
# 435 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
            ( Gt )
# 441 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
            ( Lt )
# 447 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
             ( Gte )
# 453 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
             ( Lte )
# 459 "parser.ml"
               : 'symbol))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Interpreter.program)
