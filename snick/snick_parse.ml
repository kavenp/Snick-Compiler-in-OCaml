type token =
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | FLOAT_CONST of (float)
  | STR_CONST of (string)
  | IDENT of (string)
  | BOOL
  | INT
  | FLOAT
  | AND
  | OR
  | NOT
  | IF
  | THEN
  | ELSE
  | FI
  | WHILE
  | DO
  | OD
  | PROC
  | END
  | REF
  | VAL
  | WRITE
  | READ
  | ASSIGN
  | LPAREN
  | RPAREN
  | LSQBRACKET
  | RSQBRACKET
  | DOT
  | COMMA
  | EQ
  | NOTEQ
  | LT
  | GT
  | LTEQ
  | GTEQ
  | PLUS
  | MINUS
  | MUL
  | DIV
  | SEMICOLON
  | EOF

open Parsing;;
let _ = parse_error;;
# 8 "snick_parse.mly"
open Snick_ast

let parse_error msg = Printf.eprintf "%s\n" msg
# 53 "snick_parse.ml"
let yytransl_const = [|
  262 (* BOOL *);
  263 (* INT *);
  264 (* FLOAT *);
  265 (* AND *);
  266 (* OR *);
  267 (* NOT *);
  268 (* IF *);
  269 (* THEN *);
  270 (* ELSE *);
  271 (* FI *);
  272 (* WHILE *);
  273 (* DO *);
  274 (* OD *);
  275 (* PROC *);
  276 (* END *);
  277 (* REF *);
  278 (* VAL *);
  279 (* WRITE *);
  280 (* READ *);
  281 (* ASSIGN *);
  282 (* LPAREN *);
  283 (* RPAREN *);
  284 (* LSQBRACKET *);
  285 (* RSQBRACKET *);
  286 (* DOT *);
  287 (* COMMA *);
  288 (* EQ *);
  289 (* NOTEQ *);
  290 (* LT *);
  291 (* GT *);
  292 (* LTEQ *);
  293 (* GTEQ *);
  294 (* PLUS *);
  295 (* MINUS *);
  296 (* MUL *);
  297 (* DIV *);
  298 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* BOOL_CONST *);
  258 (* INT_CONST *);
  259 (* FLOAT_CONST *);
  260 (* STR_CONST *);
  261 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\005\000\005\000\006\000\
\007\000\007\000\004\000\011\000\011\000\009\000\009\000\008\000\
\008\000\008\000\010\000\010\000\013\000\013\000\013\000\013\000\
\014\000\014\000\014\000\014\000\016\000\016\000\018\000\017\000\
\017\000\020\000\020\000\020\000\020\000\021\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\022\000\022\000\023\000\012\000\012\000\019\000\019\000\
\015\000\015\000\015\000\015\000\015\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\006\000\007\000\003\000\001\000\003\000\
\001\000\001\000\002\000\003\000\006\000\002\000\000\000\001\000\
\001\000\001\000\002\000\000\000\002\000\005\000\007\000\005\000\
\001\000\002\000\002\000\003\000\003\000\004\000\001\000\001\000\
\004\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\004\000\003\000\001\000\003\000\001\000\
\001\000\001\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\062\000\000\000\003\000\000\000\002\000\
\000\000\010\000\009\000\015\000\000\000\007\000\000\000\000\000\
\000\000\015\000\000\000\016\000\017\000\018\000\000\000\004\000\
\000\000\000\000\014\000\000\000\006\000\008\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\025\000\000\000\
\005\000\000\000\012\000\000\000\000\000\034\000\035\000\036\000\
\037\000\000\000\000\000\000\000\000\000\000\000\058\000\057\000\
\059\000\060\000\000\000\000\000\026\000\021\000\000\000\000\000\
\000\000\054\000\029\000\000\000\000\000\000\000\000\000\000\000\
\050\000\000\000\000\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\000\000\
\028\000\000\000\000\000\000\000\030\000\000\000\033\000\061\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\041\000\000\000\000\000\013\000\
\053\000\000\000\020\000\022\000\024\000\052\000\000\000\023\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\016\000\013\000\014\000\015\000\023\000\
\017\000\026\000\027\000\065\000\037\000\038\000\068\000\039\000\
\055\000\089\000\069\000\056\000\057\000\058\000\066\000"

let yysindex = "\022\000\
\243\254\000\000\029\255\000\000\243\254\000\000\038\255\000\000\
\011\255\000\000\000\000\000\000\238\254\000\000\094\255\035\255\
\094\255\000\000\089\255\000\000\000\000\000\000\070\255\000\000\
\075\255\140\000\000\000\063\255\000\000\000\000\232\254\242\254\
\066\255\066\255\066\255\080\255\000\000\045\255\000\000\079\255\
\000\000\110\255\000\000\055\255\066\255\000\000\000\000\000\000\
\000\000\088\255\066\255\066\255\066\255\039\000\000\000\000\000\
\000\000\000\000\051\000\084\000\000\000\000\000\066\255\061\255\
\043\255\000\000\000\000\084\000\251\254\047\255\137\000\072\000\
\000\000\066\255\066\255\000\000\066\255\066\255\066\255\066\255\
\066\255\066\255\066\255\066\255\066\255\066\255\000\000\084\000\
\000\000\084\255\092\255\110\255\000\000\066\255\000\000\000\000\
\137\000\094\000\074\255\068\255\068\255\068\255\068\255\068\255\
\068\255\091\255\091\255\000\000\000\000\012\255\133\255\000\000\
\000\000\084\000\000\000\000\000\000\000\000\000\135\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\136\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\118\255\000\000\000\000\000\000\000\000\000\000\114\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\086\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\098\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\010\255\000\000\000\000\254\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\099\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\182\255\126\000\000\000\187\255\210\255\233\255\238\255\005\000\
\028\000\120\255\151\255\000\000\000\000\000\000\000\000\000\000\
\000\000\034\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\138\000\124\000\000\000\125\000\000\000\128\000\
\000\000\185\255\000\000\000\000\000\000\000\000\224\255\000\000\
\230\255\000\000\101\000\000\000\000\000\000\000\056\000"

let yytablesize = 434
let yytable = "\040\000\
\054\000\059\000\060\000\042\000\099\000\003\000\051\000\051\000\
\018\000\061\000\051\000\044\000\019\000\045\000\051\000\110\000\
\032\000\043\000\071\000\072\000\073\000\093\000\001\000\033\000\
\051\000\094\000\051\000\034\000\051\000\117\000\088\000\010\000\
\011\000\007\000\035\000\036\000\056\000\012\000\056\000\051\000\
\056\000\097\000\098\000\119\000\100\000\101\000\102\000\103\000\
\104\000\105\000\106\000\107\000\108\000\109\000\024\000\046\000\
\047\000\048\000\049\000\050\000\055\000\114\000\055\000\009\000\
\055\000\051\000\046\000\047\000\048\000\049\000\050\000\091\000\
\040\000\092\000\030\000\095\000\051\000\094\000\032\000\031\000\
\052\000\067\000\041\000\040\000\050\000\033\000\062\000\115\000\
\116\000\034\000\090\000\052\000\040\000\053\000\032\000\032\000\
\035\000\036\000\032\000\020\000\021\000\022\000\032\000\063\000\
\053\000\083\000\084\000\085\000\086\000\010\000\011\000\064\000\
\032\000\111\000\032\000\045\000\032\000\032\000\032\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\038\000\038\000\085\000\086\000\038\000\112\000\118\000\001\000\
\038\000\011\000\032\000\027\000\031\000\028\000\008\000\029\000\
\025\000\070\000\038\000\113\000\038\000\000\000\038\000\038\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\039\000\
\039\000\038\000\000\000\039\000\000\000\000\000\000\000\039\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\039\000\000\000\039\000\000\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\048\000\048\000\
\039\000\000\000\048\000\042\000\042\000\000\000\048\000\042\000\
\000\000\000\000\000\000\042\000\000\000\000\000\000\000\000\000\
\048\000\000\000\048\000\000\000\048\000\042\000\000\000\042\000\
\000\000\042\000\047\000\047\000\000\000\000\000\047\000\048\000\
\000\000\000\000\047\000\000\000\042\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\000\000\047\000\000\000\
\047\000\043\000\043\000\000\000\000\000\043\000\044\000\044\000\
\000\000\043\000\044\000\047\000\000\000\000\000\044\000\000\000\
\000\000\000\000\000\000\043\000\000\000\043\000\000\000\043\000\
\044\000\000\000\044\000\000\000\044\000\045\000\045\000\000\000\
\000\000\045\000\043\000\000\000\000\000\045\000\000\000\044\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\045\000\
\000\000\045\000\020\000\045\000\046\000\046\000\000\000\000\000\
\046\000\020\000\000\000\000\000\046\000\020\000\045\000\074\000\
\075\000\020\000\000\000\076\000\020\000\020\000\046\000\000\000\
\046\000\000\000\046\000\074\000\075\000\000\000\000\000\000\000\
\000\000\000\000\000\000\087\000\000\000\046\000\077\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\085\000\086\000\
\074\000\075\000\077\000\078\000\079\000\080\000\081\000\082\000\
\083\000\084\000\085\000\086\000\074\000\075\000\000\000\000\000\
\000\000\000\000\096\000\000\000\000\000\000\000\074\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\000\000\000\000\077\000\078\000\079\000\080\000\081\000\
\082\000\083\000\084\000\085\000\086\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\086\000\049\000\
\000\000\000\000\049\000\032\000\000\000\000\000\049\000\000\000\
\032\000\000\000\033\000\000\000\000\000\120\000\034\000\033\000\
\049\000\000\000\049\000\034\000\049\000\035\000\036\000\000\000\
\000\000\000\000\035\000\036\000\000\000\000\000\000\000\049\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000"

let yycheck = "\026\000\
\033\000\034\000\035\000\028\001\076\000\019\001\009\001\010\001\
\027\001\036\000\013\001\026\001\031\001\028\001\017\001\087\000\
\005\001\042\001\051\000\052\000\053\000\027\001\001\000\012\001\
\027\001\031\001\029\001\016\001\031\001\018\001\063\000\021\001\
\022\001\005\001\023\001\024\001\027\001\027\001\029\001\042\001\
\031\001\074\000\075\000\115\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\020\001\001\001\
\002\001\003\001\004\001\005\001\027\001\094\000\029\001\026\001\
\031\001\011\001\001\001\002\001\003\001\004\001\005\001\029\001\
\099\000\031\001\005\001\029\001\011\001\031\001\005\001\005\001\
\026\001\027\001\020\001\110\000\005\001\012\001\042\001\014\001\
\015\001\016\001\030\001\026\001\119\000\039\001\009\001\010\001\
\023\001\024\001\013\001\006\001\007\001\008\001\017\001\025\001\
\039\001\038\001\039\001\040\001\041\001\021\001\022\001\002\001\
\027\001\030\001\029\001\028\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\042\001\
\009\001\010\001\040\001\041\001\013\001\042\001\002\001\000\000\
\017\001\020\001\025\001\042\001\042\001\018\000\005\000\019\000\
\017\000\045\000\027\001\092\000\029\001\255\255\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\038\001\039\001\009\001\
\010\001\042\001\255\255\013\001\255\255\255\255\255\255\017\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\027\001\255\255\029\001\255\255\031\001\032\001\033\001\
\034\001\035\001\036\001\037\001\038\001\039\001\009\001\010\001\
\042\001\255\255\013\001\009\001\010\001\255\255\017\001\013\001\
\255\255\255\255\255\255\017\001\255\255\255\255\255\255\255\255\
\027\001\255\255\029\001\255\255\031\001\027\001\255\255\029\001\
\255\255\031\001\009\001\010\001\255\255\255\255\013\001\042\001\
\255\255\255\255\017\001\255\255\042\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\027\001\255\255\029\001\255\255\
\031\001\009\001\010\001\255\255\255\255\013\001\009\001\010\001\
\255\255\017\001\013\001\042\001\255\255\255\255\017\001\255\255\
\255\255\255\255\255\255\027\001\255\255\029\001\255\255\031\001\
\027\001\255\255\029\001\255\255\031\001\009\001\010\001\255\255\
\255\255\013\001\042\001\255\255\255\255\017\001\255\255\042\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\027\001\
\255\255\029\001\005\001\031\001\009\001\010\001\255\255\255\255\
\013\001\012\001\255\255\255\255\017\001\016\001\042\001\009\001\
\010\001\020\001\255\255\013\001\023\001\024\001\027\001\255\255\
\029\001\255\255\031\001\009\001\010\001\255\255\255\255\255\255\
\255\255\255\255\255\255\017\001\255\255\042\001\032\001\033\001\
\034\001\035\001\036\001\037\001\038\001\039\001\040\001\041\001\
\009\001\010\001\032\001\033\001\034\001\035\001\036\001\037\001\
\038\001\039\001\040\001\041\001\009\001\010\001\255\255\255\255\
\255\255\255\255\027\001\255\255\255\255\255\255\009\001\032\001\
\033\001\034\001\035\001\036\001\037\001\038\001\039\001\040\001\
\041\001\255\255\255\255\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\039\001\040\001\041\001\032\001\033\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\010\001\
\255\255\255\255\013\001\005\001\255\255\255\255\017\001\255\255\
\005\001\255\255\012\001\255\255\255\255\015\001\016\001\012\001\
\027\001\255\255\029\001\016\001\031\001\023\001\024\001\255\255\
\255\255\255\255\023\001\024\001\255\255\255\255\255\255\042\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\041\001"

let yynames_const = "\
  BOOL\000\
  INT\000\
  FLOAT\000\
  AND\000\
  OR\000\
  NOT\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  WHILE\000\
  DO\000\
  OD\000\
  PROC\000\
  END\000\
  REF\000\
  VAL\000\
  WRITE\000\
  READ\000\
  ASSIGN\000\
  LPAREN\000\
  RPAREN\000\
  LSQBRACKET\000\
  RSQBRACKET\000\
  DOT\000\
  COMMA\000\
  EQ\000\
  NOTEQ\000\
  LT\000\
  GT\000\
  LTEQ\000\
  GTEQ\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  SEMICOLON\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL_CONST\000\
  INT_CONST\000\
  FLOAT_CONST\000\
  STR_CONST\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'procs) in
    Obj.repr(
# 48 "snick_parse.mly"
        ( { procs = List.rev _1 } )
# 355 "snick_parse.ml"
               : Snick_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'procs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 51 "snick_parse.mly"
               ( _2 :: _1 )
# 363 "snick_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 52 "snick_parse.mly"
               ( [_1] )
# 370 "snick_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'proc_body) in
    Obj.repr(
# 56 "snick_parse.mly"
                                           ( (_2, [], _5) )
# 378 "snick_parse.ml"
               : 'proc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'proc_args) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'proc_body) in
    Obj.repr(
# 57 "snick_parse.mly"
                                                     ( (_2, List.rev _4, _6) )
# 387 "snick_parse.ml"
               : 'proc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'proc_args) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 60 "snick_parse.mly"
                        ( _3 :: _1 )
# 395 "snick_parse.ml"
               : 'proc_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 61 "snick_parse.mly"
                        ( [_1] )
# 402 "snick_parse.ml"
               : 'proc_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg_pass_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typespec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "snick_parse.mly"
                                 ( (_1, _2, _3) )
# 411 "snick_parse.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "snick_parse.mly"
        ( Val )
# 417 "snick_parse.ml"
               : 'arg_pass_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "snick_parse.mly"
        ( Ref )
# 423 "snick_parse.ml"
               : 'arg_pass_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 71 "snick_parse.mly"
              ( (List.rev _1, List.rev _2) )
# 431 "snick_parse.ml"
               : 'proc_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 75 "snick_parse.mly"
                             ( RegDecl (_2, _1) )
# 439 "snick_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'intervals) in
    Obj.repr(
# 76 "snick_parse.mly"
                                                             ( ArrayDecl (_2, _1, List.rev _4) )
# 448 "snick_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 79 "snick_parse.mly"
               ( _2 :: _1 )
# 456 "snick_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "snick_parse.mly"
    ( [] )
# 462 "snick_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "snick_parse.mly"
         ( Bool )
# 468 "snick_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "snick_parse.mly"
        ( Int )
# 474 "snick_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "snick_parse.mly"
          ( Float )
# 480 "snick_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 89 "snick_parse.mly"
               ( _2 :: _1 )
# 488 "snick_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "snick_parse.mly"
    ( [] )
# 494 "snick_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_body) in
    Obj.repr(
# 93 "snick_parse.mly"
                        ( _1 )
# 501 "snick_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 94 "snick_parse.mly"
                          ( Ifthen (_2, _4) )
# 509 "snick_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 95 "snick_parse.mly"
                                     ( IfthenElse (_2, _4, _6) )
# 518 "snick_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 96 "snick_parse.mly"
                           ( WhileDo (_2, _4) )
# 526 "snick_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_call) in
    Obj.repr(
# 99 "snick_parse.mly"
              ( ProcCall _1 )
# 533 "snick_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 100 "snick_parse.mly"
                ( Read _2 )
# 540 "snick_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "snick_parse.mly"
               ( Write _2 )
# 547 "snick_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 102 "snick_parse.mly"
                         ( Assign (_1, _3) )
# 555 "snick_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 106 "snick_parse.mly"
                              ( (_1, []) )
# 562 "snick_parse.ml"
               : 'proc_call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 107 "snick_parse.mly"
                              ( (_1, List.rev _3) )
# 570 "snick_parse.ml"
               : 'proc_call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "snick_parse.mly"
         ( Rexpr _1 )
# 577 "snick_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "snick_parse.mly"
          ( LId _1 )
# 584 "snick_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 115 "snick_parse.mly"
                                      ( LArray (_1, List.rev _3) )
# 592 "snick_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 118 "snick_parse.mly"
               ( Ebool _1 )
# 599 "snick_parse.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 119 "snick_parse.mly"
              ( Eint _1 )
# 606 "snick_parse.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 120 "snick_parse.mly"
                ( Efloat _1 )
# 613 "snick_parse.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "snick_parse.mly"
              ( Estring _1 )
# 620 "snick_parse.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "snick_parse.mly"
                   ( Ebinop (_1, Op_add, _3) )
# 628 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "snick_parse.mly"
                    ( Ebinop (_1, Op_sub, _3) )
# 636 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "snick_parse.mly"
                  ( Ebinop (_1, Op_mul, _3) )
# 644 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "snick_parse.mly"
                  ( Ebinop (_1, Op_div, _3) )
# 652 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "snick_parse.mly"
                 ( Ebinop (_1, Op_eq, _3) )
# 660 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "snick_parse.mly"
                 ( Ebinop (_1, Op_lt, _3) )
# 668 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "snick_parse.mly"
                 ( Ebinop (_1, Op_gt, _3) )
# 676 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "snick_parse.mly"
                   ( Ebinop (_1, Op_lteq, _3) )
# 684 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "snick_parse.mly"
                   ( Ebinop (_1, Op_gteq, _3) )
# 692 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "snick_parse.mly"
                    ( Ebinop (_1, Op_noteq, _3) )
# 700 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "snick_parse.mly"
                  ( Ebinop (_1, Op_and, _3) )
# 708 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "snick_parse.mly"
                 ( Ebinop (_1, Op_or, _3) )
# 716 "snick_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "snick_parse.mly"
                            ( Eunop (Op_minus, _2) )
# 723 "snick_parse.ml"
               : 'unop))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "snick_parse.mly"
                        ( Eunop (Op_not, _2) )
# 730 "snick_parse.ml"
               : 'unop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 146 "snick_parse.mly"
                                ( Interval (_1, _4) )
# 738 "snick_parse.ml"
               : 'interval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'intervals) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'interval) in
    Obj.repr(
# 153 "snick_parse.mly"
                             ( _3 :: _1 )
# 746 "snick_parse.ml"
               : 'intervals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'interval) in
    Obj.repr(
# 154 "snick_parse.mly"
             ( [_1] )
# 753 "snick_parse.ml"
               : 'intervals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exprs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "snick_parse.mly"
                     ( _3 :: _1 )
# 761 "snick_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "snick_parse.mly"
                     ( [_1] )
# 768 "snick_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 162 "snick_parse.mly"
            ( _1 )
# 775 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 163 "snick_parse.mly"
           ( Elval _1 )
# 782 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop) in
    Obj.repr(
# 165 "snick_parse.mly"
          ( _1 )
# 789 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unop) in
    Obj.repr(
# 166 "snick_parse.mly"
         ( _1 )
# 796 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 167 "snick_parse.mly"
                       ( _2 )
# 803 "snick_parse.ml"
               : 'expr))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Snick_ast.program)
