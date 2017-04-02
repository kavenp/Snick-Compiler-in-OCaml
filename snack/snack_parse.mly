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
%nonassoc UNOT
%nonassoc EQ LT GT LTEQ GTEQ NOTEQ
%left PLUS MINUS 
%left MUL DIV
%nonassoc UMINUS

%type <Snack_ast.program> program

%start program
%%
program:
  procs { { procs = List.rev $1 } }

procs:
  | procs proc { $2 :: $1 }
  | { [] }

proc:
  /* Empty process header */
  | PROC IDENT LPAREN RPAREN proc_body END { ($2, [], $5) }
  | PROC IDENT LPAREN proc_args RPAREN proc_body END { ($2, List.rev $4,$6) }

proc_args:
  | proc_args COMMA arg { $3 :: $1 }
  | { [] }

arg:
  | arg_pass_type typespec IDENT { ($1, $2, $3) }

arg_pass_type:
  | VAL { Val }
  | REF { Ref }

proc_body:
  decls stmts { (List.rev $1, List.rev $2) }

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

literal:
  | BOOL_CONST { Ebool $1 }
  | INT_CONST { Eint $1 }
  | FLOAT_CONST { Efloat $1 }
  | STR_CONST { Estring $1 }

binop:
  | expr PLUS expr { Ebinop ($1, Op_add, $3) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
  | expr MUL expr { Ebinop ($1, Op_mul, $3) }
  | expr DIV expr { Ebinop ($1, Op_div, $3) }
  /* Comparison */
  | expr EQ expr { Ebinop ($1, Op_eq, $3) }
  | expr LT expr { Ebinop ($1, Op_lt, $3) }
  | expr GT expr { Ebinop ($1, Op_gt, $3) }
  | expr LTEQ expr { Ebinop ($1, Op_lteq, $3) }
  | expr GTEQ expr { Ebinop ($1, Op_gteq, $3) }
  | expr NOTEQ expr { Ebinop ($1, Op_noteq, $3) }
  /* boolean op */
  | expr AND expr { Ebinop ($1, Op_and, $3) }
  | expr OR expr { Ebinop ($1, Op_or, $3) }

unop:
  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }
  | NOT expr %prec UNOT { Eunop (Op_not, $2) }

interval:
  | expr DOT DOT expr { Ebinop ($1, Op_interval, $4) }

/* list of expressions */
exprs:
  | exprs expr { $2 :: $1 }
  | expr { [$1] }

expr:
  | literal { $1 }
  | lvalue { Elval $1 }
  | interval { $1 }
  /* Binary operators */
  | binop { $1 }
  | unop { $1 }
  | LPAREN expr RPAREN { $2 }
  | IDENT LSQBRACKET exprs RSQBRACKET { ArrayOp ($1,$3) }
