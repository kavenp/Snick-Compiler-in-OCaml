/* ocamlyacc parser for bean */
%{
open Snack_ast

let parse_error msg = Printf.eprintf "%s\n" msg
%}

%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token <string> STR_CONST
%token <string> IDENT
%token BOOL INT FLOAT
%token AND OR NOT
%token IF THEN ELSE FI
%token WHILE DO OD
%token PROC END
%token REF VAL
%token WRITE READ
%token ASSIGN
%token LPAREN RPAREN
%token LSQBRACKET RSQBRACKET
%token DOT COMMA
%token EQ NOTEQ LT GT
%token LTEQ GTEQ
%token PLUS MINUS MUL DIV
%token SEMICOLON
%token EOF

%left OR
%left AND
%left NOT
%nonassoc EQ LT GT LTEQ GTEQ NOTEQ
%left PLUS MINUS 
%left MUL DIV
%nonassoc UMINUS

%type <Snack_ast.program> program

%start program
%%

program:
  decls stmts { { decls = List.rev $1 ; stmts = List.rev $2 } }

decl :
  | typespec IDENT SEMICOLON { ($2, $1) }

decls :
  | decls decl { $2 :: $1 }
  | { [] }

typespec :
  | BOOL { Bool }
  | INT { Int }
  | FLOAT { Float }

/* Builds stmts in reverse order */
stmts:
  | stmts stmt { $2 :: $1 }
  | { [] }

stmt :
  stmt_body SEMICOLON { $1 }

stmt_body:
  | READ lvalue { Read $2 }
  | WRITE expr { Write $2 }
  | lvalue ASSIGN rvalue { Assign ($1, $3) }

rvalue :
  | expr { Rexpr $1 }

lvalue:
  | IDENT { LId $1 }

expr:
  | BOOL_CONST { Ebool $1 }
  | INT_CONST { Eint $1 }
  | FLOAT_CONST { Efloat $1 }
  | STR_CONST { Estring $1 }
  | lvalue { Elval $1 }
  /* Binary operators */
  | expr PLUS expr { Ebinop ($1, Op_add, $3) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
  | expr MUL expr { Ebinop ($1, Op_mul, $3) }
  | expr EQ expr { Ebinop ($1, Op_eq, $3) }
  | expr LT expr { Ebinop ($1, Op_lt, $3) }
  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }
  | LPAREN expr RPAREN { $2 }
