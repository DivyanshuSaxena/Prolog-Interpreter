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
open Printf
# 39 "parser.ml"
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
\001\000\001\000\002\000\003\000\003\000\005\000\005\000\005\000\
\004\000\004\000\006\000\006\000\006\000\007\000\007\000\007\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\000\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\003\000\001\000\003\000\002\000\004\000\
\003\000\001\000\004\000\001\000\001\000\003\000\003\000\001\000\
\001\000\001\000\001\000\002\000\003\000\002\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\012\000\013\000\000\000\001\000\
\035\000\000\000\000\000\000\000\036\000\003\000\000\000\000\000\
\000\000\002\000\000\000\000\000\007\000\000\000\019\000\023\000\
\017\000\018\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\000\000\000\000\000\000\
\000\000\006\000\004\000\000\000\009\000\020\000\000\000\011\000\
\000\000\000\000\022\000\008\000\021\000\015\000\014\000"

let yydgoto = "\003\000\
\009\000\013\000\010\000\014\000\011\000\015\000\039\000\040\000\
\041\000"

let yysindex = "\034\000\
\005\000\001\255\000\000\000\255\000\000\000\000\001\255\000\000\
\000\000\005\000\006\255\004\255\000\000\000\000\011\255\047\255\
\236\254\000\000\254\254\001\255\000\000\001\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\255\010\255\253\254\
\047\255\000\000\000\000\241\254\000\000\000\000\029\255\000\000\
\047\255\047\255\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\009\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\045\000\000\000\048\000\255\255\000\000\003\000\230\255\018\000\
\000\000"

let yytablesize = 292
let yytable = "\004\000\
\010\000\049\000\004\000\012\000\008\000\017\000\016\000\005\000\
\042\000\020\000\019\000\047\000\012\000\052\000\051\000\022\000\
\016\000\048\000\044\000\050\000\045\000\012\000\054\000\005\000\
\006\000\007\000\005\000\006\000\023\000\024\000\025\000\026\000\
\021\000\016\000\001\000\002\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\037\000\023\000\
\024\000\025\000\026\000\038\000\046\000\053\000\018\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\043\000\055\000\000\000\000\000\038\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\005\000\006\000\
\007\000\005\000\005\000\005\000"

let yycheck = "\002\001\
\000\000\005\001\002\001\001\000\000\000\007\000\007\001\000\000\
\029\001\006\001\005\001\038\000\010\000\029\001\041\000\005\001\
\008\001\008\001\020\000\023\001\022\000\019\000\049\000\026\001\
\027\001\028\001\026\001\027\001\001\001\002\001\003\001\004\001\
\029\001\025\001\001\000\002\000\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\001\001\
\002\001\003\001\004\001\024\001\025\001\025\001\010\000\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\019\000\050\000\255\255\255\255\024\001\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\002\001\255\255\
\255\255\002\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\029\001\026\001\027\001\
\028\001\026\001\027\001\028\001"

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
# 23 "parser.mly"
                ( [] )
# 246 "parser.ml"
               : Interpreter.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clauselist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Interpreter.program) in
    Obj.repr(
# 24 "parser.mly"
                      ( (_1)@(_2) )
# 254 "parser.ml"
               : Interpreter.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomlist) in
    Obj.repr(
# 26 "parser.mly"
                    ( Query(_1) )
# 261 "parser.ml"
               : Interpreter.clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'clauselist) in
    Obj.repr(
# 28 "parser.mly"
                                    ( (_1)::(_3) )
# 269 "parser.ml"
               : 'clauselist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 29 "parser.mly"
                ( [_1] )
# 276 "parser.ml"
               : 'clauselist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomlist) in
    Obj.repr(
# 31 "parser.mly"
                             ( Query(_2) )
# 283 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 32 "parser.mly"
                 ( Fact(_1) )
# 290 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'atomlist) in
    Obj.repr(
# 33 "parser.mly"
                           ( Rule(_1,_3) )
# 298 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomlist) in
    Obj.repr(
# 35 "parser.mly"
                               ( (_1)::(_3) )
# 306 "parser.ml"
               : 'atomlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 36 "parser.mly"
               ( [_1] )
# 313 "parser.ml"
               : 'atomlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'termlist) in
    Obj.repr(
# 38 "parser.mly"
                                 ( PredSym(_1,_3) )
# 321 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
              ( Cut )
# 327 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
               ( Fail )
# 333 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 42 "parser.mly"
                              ( (_1)::[_3] )
# 341 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'termlist) in
    Obj.repr(
# 43 "parser.mly"
                         ( (_1)::(_3) )
# 349 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 44 "parser.mly"
               ( [_1] )
# 356 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 46 "parser.mly"
                  ( Const(_1) )
# 363 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 47 "parser.mly"
               ( Bool(_1) )
# 370 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "parser.mly"
              ( Var(_1,0) )
# 377 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                       ( FuncSym(List,[]) )
# 383 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'termlist) in
    Obj.repr(
# 50 "parser.mly"
                              ( FuncSym(List,_2) )
# 390 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'symbol) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'termlist) in
    Obj.repr(
# 51 "parser.mly"
                      ( FuncSym(_1,_2) )
# 398 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
            ( Cons(_1) )
# 405 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                  ( Plus )
# 411 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
               ( Minus )
# 417 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
               ( Prod )
# 423 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
              ( Div )
# 429 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
              ( Mod )
# 435 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
             ( Exp )
# 441 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
            ( Eq )
# 447 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
            ( Gt )
# 453 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
            ( Lt )
# 459 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
             ( Gte )
# 465 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
             ( Lte )
# 471 "parser.ml"
               : 'symbol))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry goal *)
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
let goal (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Interpreter.clause)
