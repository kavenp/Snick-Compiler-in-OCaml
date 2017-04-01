(* Specification of an AST for Snack *)
type ident = string
 
(* Keep aliases intact for pretty printing. *)
type snacktype =
  | Bool
  | Int
  | Float

type typedef = (ident * snacktype)

type lvalue =
  | LId of ident
  | LField of (lvalue * ident)

type binop =
  | Op_sub | Op_mul | Op_div | Op_eq | Op_lt | Op_gt 
  | Op_noteq | Op_gteq | Op_lteq

type unop =
  | Op_minus

type expr =
  | Ebool of bool
  | Eint of int
  | Efloat of float
  | Estring of string
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

type decl = (ident * snacktype)

type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr

type program = {
  decls : typedef list ;
  stmts : stmt list
}
 
type t = program
