{
open Snick_parse

exception Lex_error of string
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let digits = digit+
let ident = (alpha | '_') alnum*
let str = '"' [^ '\n' '\t' '"']* '"'
rule token = parse
  | [' ' '\t']    { token lexbuf }     (* skip blanks *)
  | '\n'          { Lexing.new_line lexbuf ; token lexbuf }
  | '-'? ('0' | ['1'-'9']+['0'-'9']*) '.' digits as lxm { FLOAT_CONST(float_of_string lxm) }
  | '-'? ('0' | ['1'-'9']+['0'-'9']*) as lxm { INT_CONST(int_of_string lxm) }
  | '#'[^ '\n']* { token lexbuf }   (* skip comments *)
  | str as lxm { STR_CONST lxm }
  (* keywords *)
  | "and" { AND }
  | "do" { DO }
  | "od" { OD }
  | "if" { IF }
  | "fi" { FI }
  | "end" { END }
  | "else" { ELSE }
  | "not" {NOT}
  | "or" { OR }
  | "proc" { PROC }
  | "ref" { REF }
  | "then" { THEN }
  | "val" { VAL }
  | "while" { WHILE }
  | "bool" { BOOL }
  | "int" { INT }
  | "float" { FLOAT }
  | "true" { BOOL_CONST true }
  | "false" { BOOL_CONST false }
  | "read" { READ }
  | "write" { WRITE }
  | ":=" { ASSIGN }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LSQBRACKET }
  | ']' { RSQBRACKET }
  | '.' { DOT }
  | ',' { COMMA }
  | "!=" { NOTEQ }
  | '=' { EQ }
  | "<=" { LTEQ }
  | '<' { LT }
  | ">=" { GTEQ }
  | '>' { GT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MUL }
  | '/' { DIV }
  | ';' { SEMICOLON }
  | ident as lxm { IDENT lxm }
  | eof { EOF }
  | _ { raise (Lex_error("Unknown symbol \"" ^ (Lexing.lexeme lexbuf) ^ "\"")) }
