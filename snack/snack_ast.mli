(* Specification of an AST for Snack *)
type ident = string
 
(* Keep aliases intact for pretty printing. *)
type snacktype =
  | Bool
  | Int
  | Float

type arg_pass_type = 
  | Val | Ref

type typedef = (ident * snacktype)

type lvalue =
  | LId of ident
  | LField of (lvalue * ident)

type binop =
  |Op_add | Op_sub | Op_mul | Op_div | Op_eq | Op_lt | Op_gt 
  | Op_noteq | Op_gteq | Op_lteq | Op_and | Op_or | Op_interval

type unop =
  | Op_minus
  | Op_not

type expr =
  | Ebool of bool
  | Eint of int
  | Efloat of float
  | Estring of string
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)
  | ArrayOp of (ident * expr list)

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

type decl = (ident * snacktype)

type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | Ifthen of (expr * stmt list)
  | IfthenElse of (expr * stmt list * stmt list)
  | WhileDo of (expr * stmt list)

type arg = (arg_pass_type * snacktype * ident)

type proc_body = (decl list * stmt list)

type proc = (ident * arg list * proc_body)

type program = {
  procs : proc list
}
 
type t = program
