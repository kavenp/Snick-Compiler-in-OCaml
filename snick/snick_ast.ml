(* ----------------------------------------------------- | 
 * Abstract Syntax Tree for Snick language               |
 * ----------------------------------------------------- |
 * Tree representation of Snick program in program       |
 * built by the Snick parser                             |
 * ----------------------------------------------------- | *)

type pos = (Lexing.position * Lexing.position)

type ident = string
 
(* Keep aliases intact for pretty printing. *)
type snicktype =
  | Bool | Int | Float

type arg_pass_type = 
  | Val | Ref

type binop =
  | Op_add | Op_sub | Op_mul | Op_div | Op_eq | Op_lt | Op_gt | Op_noteq 
  | Op_gteq | Op_lteq | Op_and | Op_or

type unop =
  | Op_minus
  | Op_not

(* Mutually recursive types expr and lvalue *)
type expr =
  | Ebool of (bool * pos)
  | Eint of (int * pos)
  | Efloat of (string * pos)
  | Elval of (lvalue * pos)
  | Ebinop of (expr * binop * expr * pos)
  | Eunop of (unop * expr * pos)
and lvalue =
  | LId of (ident * pos)
  | LArray of (ident * expr list * pos)

type writeable =
  | WExpr of expr
  | WString of (string * pos)

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

(* First int is lower bound, Second int is upper bound *)
type interval = 
  | Interval of (int * int * pos)

type decl = 
  | RegDecl of (ident * snicktype * pos)
  | ArrayDecl of (ident * snicktype * interval list * pos)

type stmt = 
  | Assign of (lvalue * rvalue * pos)
  | Read of lvalue
  | Write of writeable
  | Ifthen of (expr * stmt list)
  | IfthenElse of (expr * stmt list * stmt list)
  | WhileDo of (expr * stmt list)
  | ProcCall of (ident * expr list * pos)

type arg = (arg_pass_type * snicktype * ident * pos)

type proc_body = (decl list * stmt list)

type proc = (ident * arg list * proc_body * pos)

type program = {
  procs : proc list
}
 
type t = program

(*===== Position Helper Functions =====*)
let get_pos_info (start_pos, end_pos) =
  let get_line_col pos = 
    let start_line  = pos.Lexing.pos_lnum in
    let start_col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
    (start_line, start_col)
  in
  let start_p = get_line_col start_pos in
  let end_p   = get_line_col end_pos in
  (start_p, end_p)

let get_lval_pos lval =
  match lval with
  | LId (_, pos) -> pos
  | LArray (_, _, pos) -> pos