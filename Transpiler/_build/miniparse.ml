type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | AND
  | OR
  | IF
  | FOR
  | WHILE
  | INT
  | BOOL
  | RETURN
  | COMMA
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "miniparse.mly"
open Ast
# 34 "miniparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* TIMES *);
  265 (* DIV *);
  266 (* ASSIGN *);
  267 (* EQ *);
  268 (* NEQ *);
  269 (* LT *);
  270 (* AND *);
  271 (* OR *);
  272 (* IF *);
  273 (* FOR *);
  274 (* WHILE *);
  275 (* INT *);
  276 (* BOOL *);
  277 (* RETURN *);
  278 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  279 (* LITERAL *);
  280 (* BLIT *);
  281 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\004\000\007\000\007\000\009\000\009\000\008\000\008\000\
\010\000\010\000\010\000\010\000\010\000\010\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\012\000\012\000\013\000\
\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\008\000\000\000\001\000\001\000\003\000\000\000\002\000\
\002\000\003\000\005\000\009\000\005\000\003\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\042\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\004\000\007\000\003\000\000\000\
\000\000\012\000\000\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\024\000\000\000\000\000\000\000\000\000\006\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\016\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\018\000\000\000\000\000\000\000\022\000\
\000\000\000\000\039\000\000\000\000\000\000\000\028\000\029\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\019\000\000\000\021\000\041\000\000\000\000\000\
\000\000\020\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\024\000\009\000\017\000\035\000\
\018\000\036\000\037\000\066\000\067\000"

let yysindex = "\010\000\
\023\255\000\000\000\000\000\000\000\000\023\000\066\255\023\255\
\009\255\000\000\023\255\023\255\000\000\000\000\000\000\022\255\
\056\255\000\000\023\255\059\255\000\000\023\255\064\255\058\255\
\023\255\016\255\058\255\069\255\070\255\071\255\016\255\000\000\
\000\000\004\255\075\255\058\255\189\255\000\000\247\255\082\255\
\016\255\016\255\016\255\204\255\016\255\016\255\000\000\000\000\
\000\000\016\255\016\255\016\255\016\255\016\255\016\255\016\255\
\016\255\016\255\000\000\000\000\004\000\219\255\017\000\000\000\
\172\255\085\255\000\000\040\000\061\255\061\255\000\000\000\000\
\039\255\039\255\001\255\059\000\050\000\058\255\016\255\058\255\
\016\255\000\000\000\000\234\255\000\000\000\000\016\255\030\000\
\058\255\000\000"

let yyrindex = "\000\000\
\093\000\000\000\000\000\000\000\000\000\000\000\000\000\093\000\
\000\000\000\000\093\000\096\255\000\000\000\000\000\000\097\255\
\000\000\000\000\000\000\000\000\000\000\033\255\000\000\099\255\
\033\255\000\000\099\255\000\000\000\000\000\000\000\000\000\000\
\000\000\083\255\000\000\099\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\105\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\106\255\000\000\000\000\000\255\100\255\117\255\000\000\000\000\
\137\255\152\255\132\255\063\255\155\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\025\000\150\000\000\000\077\000\000\000\000\000\241\255\
\091\000\180\255\230\255\000\000\035\000"

let yytablesize = 328
let yytable = "\039\000\
\035\000\083\000\035\000\085\000\044\000\045\000\050\000\051\000\
\052\000\053\000\001\000\040\000\090\000\046\000\061\000\062\000\
\063\000\026\000\065\000\068\000\048\000\035\000\010\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\013\000\014\000\005\000\015\000\005\000\005\000\032\000\033\000\
\034\000\003\000\004\000\019\000\050\000\051\000\052\000\053\000\
\005\000\005\000\005\000\056\000\084\000\005\000\065\000\005\000\
\005\000\005\000\020\000\026\000\088\000\027\000\022\000\033\000\
\025\000\033\000\011\000\012\000\052\000\053\000\041\000\042\000\
\043\000\028\000\029\000\030\000\033\000\033\000\031\000\047\000\
\032\000\033\000\034\000\025\000\033\000\025\000\060\000\082\000\
\025\000\025\000\025\000\025\000\002\000\025\000\025\000\025\000\
\025\000\025\000\011\000\013\000\026\000\038\000\026\000\015\000\
\025\000\026\000\026\000\038\000\040\000\021\000\026\000\026\000\
\026\000\026\000\026\000\086\000\000\000\027\000\000\000\027\000\
\000\000\026\000\027\000\027\000\000\000\000\000\000\000\027\000\
\027\000\027\000\027\000\027\000\032\000\000\000\032\000\000\000\
\000\000\030\000\027\000\030\000\000\000\000\000\032\000\032\000\
\032\000\032\000\032\000\030\000\030\000\000\000\030\000\030\000\
\031\000\032\000\031\000\034\000\000\000\034\000\030\000\000\000\
\000\000\016\000\031\000\031\000\000\000\031\000\031\000\000\000\
\016\000\034\000\000\000\023\000\000\000\031\000\023\000\000\000\
\034\000\050\000\051\000\052\000\053\000\000\000\054\000\055\000\
\056\000\057\000\058\000\000\000\000\000\049\000\000\000\000\000\
\000\000\081\000\050\000\051\000\052\000\053\000\000\000\054\000\
\055\000\056\000\057\000\058\000\064\000\000\000\000\000\000\000\
\000\000\050\000\051\000\052\000\053\000\000\000\054\000\055\000\
\056\000\057\000\058\000\079\000\000\000\000\000\000\000\000\000\
\050\000\051\000\052\000\053\000\000\000\054\000\055\000\056\000\
\057\000\058\000\087\000\000\000\000\000\000\000\000\000\050\000\
\051\000\052\000\053\000\000\000\054\000\055\000\056\000\057\000\
\058\000\059\000\000\000\000\000\050\000\051\000\052\000\053\000\
\000\000\054\000\055\000\056\000\057\000\058\000\078\000\000\000\
\000\000\050\000\051\000\052\000\053\000\000\000\054\000\055\000\
\056\000\057\000\058\000\080\000\000\000\000\000\050\000\051\000\
\052\000\053\000\000\000\054\000\055\000\056\000\057\000\058\000\
\089\000\000\000\000\000\050\000\051\000\052\000\053\000\000\000\
\054\000\055\000\056\000\057\000\058\000\050\000\051\000\052\000\
\053\000\000\000\054\000\055\000\056\000\057\000\058\000\050\000\
\051\000\052\000\053\000\000\000\054\000\055\000\056\000\057\000\
\050\000\051\000\052\000\053\000\000\000\054\000\055\000\056\000"

let yycheck = "\026\000\
\001\001\078\000\003\001\080\000\031\000\002\001\006\001\007\001\
\008\001\009\001\001\000\027\000\089\000\010\001\041\000\042\000\
\043\000\002\001\045\000\046\000\036\000\022\001\000\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\008\000\025\001\002\001\011\000\004\001\005\001\023\001\024\001\
\025\001\019\001\020\001\022\001\006\001\007\001\008\001\009\001\
\016\001\017\001\018\001\013\001\079\000\021\001\081\000\023\001\
\024\001\025\001\003\001\002\001\087\000\004\001\004\001\001\001\
\001\001\003\001\001\001\002\001\008\001\009\001\002\001\002\001\
\002\001\016\001\017\001\018\001\014\001\015\001\021\001\005\001\
\023\001\024\001\025\001\001\001\022\001\003\001\005\001\003\001\
\006\001\007\001\008\001\009\001\000\000\011\001\012\001\013\001\
\014\001\015\001\003\001\003\001\001\001\025\000\003\001\005\001\
\022\001\006\001\007\001\003\001\003\001\019\000\011\001\012\001\
\013\001\014\001\015\001\081\000\255\255\001\001\255\255\003\001\
\255\255\022\001\006\001\007\001\255\255\255\255\255\255\011\001\
\012\001\013\001\014\001\015\001\001\001\255\255\003\001\255\255\
\255\255\001\001\022\001\003\001\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\011\001\012\001\255\255\014\001\015\001\
\001\001\022\001\003\001\001\001\255\255\003\001\022\001\255\255\
\255\255\012\000\011\001\012\001\255\255\014\001\015\001\255\255\
\019\000\015\001\255\255\022\000\255\255\022\001\025\000\255\255\
\022\001\006\001\007\001\008\001\009\001\255\255\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\001\001\255\255\255\255\
\255\255\022\001\006\001\007\001\008\001\009\001\255\255\011\001\
\012\001\013\001\014\001\015\001\001\001\255\255\255\255\255\255\
\255\255\006\001\007\001\008\001\009\001\255\255\011\001\012\001\
\013\001\014\001\015\001\001\001\255\255\255\255\255\255\255\255\
\006\001\007\001\008\001\009\001\255\255\011\001\012\001\013\001\
\014\001\015\001\001\001\255\255\255\255\255\255\255\255\006\001\
\007\001\008\001\009\001\255\255\011\001\012\001\013\001\014\001\
\015\001\003\001\255\255\255\255\006\001\007\001\008\001\009\001\
\255\255\011\001\012\001\013\001\014\001\015\001\003\001\255\255\
\255\255\006\001\007\001\008\001\009\001\255\255\011\001\012\001\
\013\001\014\001\015\001\003\001\255\255\255\255\006\001\007\001\
\008\001\009\001\255\255\011\001\012\001\013\001\014\001\015\001\
\003\001\255\255\255\255\006\001\007\001\008\001\009\001\255\255\
\011\001\012\001\013\001\014\001\015\001\006\001\007\001\008\001\
\009\001\255\255\011\001\012\001\013\001\014\001\015\001\006\001\
\007\001\008\001\009\001\255\255\011\001\012\001\013\001\014\001\
\006\001\007\001\008\001\009\001\255\255\011\001\012\001\013\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  AND\000\
  OR\000\
  IF\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  RETURN\000\
  COMMA\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 32 "miniparse.mly"
            ( _1)
# 259 "miniparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "miniparse.mly"
                 ( ([], [])               )
# 265 "miniparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 36 "miniparse.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 273 "miniparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 37 "miniparse.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 281 "miniparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "miniparse.mly"
              ( [] )
# 287 "miniparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 41 "miniparse.mly"
                           (  _1 :: _3 )
# 295 "miniparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "miniparse.mly"
         ( (_1, _2) )
# 303 "miniparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "miniparse.mly"
          ( Int   )
# 309 "miniparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "miniparse.mly"
          ( Bool  )
# 315 "miniparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 54 "miniparse.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals=_3;
      locals=_6;
      body=_7
    }
  )
# 333 "miniparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "miniparse.mly"
              ( [] )
# 339 "miniparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 67 "miniparse.mly"
                 ( _1 )
# 346 "miniparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 70 "miniparse.mly"
        ( [_1] )
# 353 "miniparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 71 "miniparse.mly"
                             ( _1::_3 )
# 361 "miniparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "miniparse.mly"
                ( [] )
# 367 "miniparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 75 "miniparse.mly"
                    ( _1::_2 )
# 375 "miniparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 78 "miniparse.mly"
                                            ( Expr _1      )
# 382 "miniparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 79 "miniparse.mly"
                                            ( Block _2 )
# 389 "miniparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "miniparse.mly"
                                  ( If(_3, _5) )
# 397 "miniparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "miniparse.mly"
                                                    ( For (_3, _5, _7, _9) )
# 407 "miniparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 84 "miniparse.mly"
                                            ( While (_3, _5)  )
# 415 "miniparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 86 "miniparse.mly"
                                            ( Return _2      )
# 422 "miniparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 89 "miniparse.mly"
                     ( Literal(_1)            )
# 429 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 90 "miniparse.mly"
                     ( BoolLit(_1)            )
# 436 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "miniparse.mly"
                     ( Id(_1)                 )
# 443 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "miniparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 451 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "miniparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 459 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "miniparse.mly"
                     ( Binop(_1, Mul,   _3)   )
# 467 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "miniparse.mly"
                     ( Binop(_1, Div,   _3)   )
# 475 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "miniparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 483 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "miniparse.mly"
                     ( Binop(_1, Neq, _3)     )
# 491 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "miniparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 499 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "miniparse.mly"
                     ( Binop(_1, And,   _3)   )
# 507 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "miniparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 515 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "miniparse.mly"
                     ( Assign(_1, _3)         )
# 523 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "miniparse.mly"
                       ( _2                   )
# 530 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 104 "miniparse.mly"
                              ( Call (_1, _3)  )
# 538 "miniparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "miniparse.mly"
              ( [] )
# 544 "miniparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 109 "miniparse.mly"
         ( _1 )
# 551 "miniparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "miniparse.mly"
        ( [_1] )
# 558 "miniparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 113 "miniparse.mly"
                    ( _1::_3 )
# 566 "miniparse.ml"
               : 'args))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
