type token =
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | FLOAT_CONST of (string)
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
  | DOUBLEDOT
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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Snick_ast.program
