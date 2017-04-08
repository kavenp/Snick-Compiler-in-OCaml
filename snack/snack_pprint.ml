open Snack_ast
open Format

let print_program fmt prog = ()
    (* fprintf fmt "@[<v>--@;<0 2>@[<v>(---%( fmt %)@;--)@]@;--@]@." "@;" example of indentation using format *)

(* Binary operator strings *)
let string_of_binop binop =
  match binop with
  | Op_add  -> "+"
  | Op_sub  -> "-"
  | Op_mul  -> "*"
  | Op_div  -> "/"
  | Op_and  -> "and"
  | Op_or   -> "or"
  | Op_eq   -> "="
  | Op_noteq  -> "!="
  | Op_lt   -> "<"
  | Op_lteq  -> "<="
  | Op_gt   -> ">"
  | Op_gteq  -> ">="

(* Unary operator strings *)
let string_of_unop unop =
  match unop with
  | Op_minus -> "-"
  | Op_not -> "not"

(* Add brackets around a string *)
let add_brackets str =
  String.concat str ["(";")"]

(* Add square brackets around string *used for arrays*)
let add_sqBrackets str =
  String.concat str ["[";"]"]

(* Set precedences of operators to help with printing brackets *)
let op_prec expr =
  match expr with
  | Ebinop (_, Op_or, _)    -> 1
  | Ebinop (_, Op_and, _)   -> 2
  | Eunop  (Op_not, _)      -> 3
  | Ebinop (_, Op_eq, _)
  | Ebinop (_, Op_noteq, _)
  | Ebinop (_, Op_lt, _)
  | Ebinop (_, Op_lteq, _)
  | Ebinop (_, Op_gt, _)
  | Ebinop (_, Op_gteq, _)  -> 4
  | Ebinop (_, Op_add, _)
  | Ebinop (_, Op_sub, _)   -> 5
  | Ebinop (_, Op_mul, _)
  | Ebinop (_, Op_div, _)   -> 6
  | Eunop  ( Op_minus, _)   -> 7
  | _                       -> 8 
  (* Other exprs are higher precedence than all here *)

(* Convert expression to string *)
let rec string_of_expr expr =
  match expr with
  | Ebool ebool -> string_of_bool ebool
  | Eint eint -> string_of_int eint
  | Efloat efloat -> string_of_float efloat
  | Estring estring -> estring
  | Elval elval -> string_of_lval elval
  | Ebinop (lexpr, binop, rexpr) -> string_of_binop_expr lexpr binop rexpr
  | Eunop (unop, expr) -> string_of_unop_expr unop expr
and
(* Convert lval to string, using and here because lval and expr types are mutually recursive *)
string_of_lval lval =
  match lval with
  | LId ident -> ident
  | LArray (ident, exprs) 
    -> ident ^ (add_sqBrackets (String.concat ", " (List.map string_of_expr exprs)))
  (* Concat ident to front of a square bracketed list of comma separated expressions converted to string *)
and
(* Convert binary operations to string by
 * taking into account the precedence of left and right expressions
 * and adding brackets as needed to maintain the meaning of
 * the main expression stored in AST *)
string_of_binop_expr lexpr binop rexpr =
  let mainExpr = Ebinop (lexpr, binop, rexpr)
  in
  String.concat " " [bracket_binop mainExpr lexpr;
                     string_of_binop binop; 
                     bracket_binop mainExpr rexpr ~isRight:true]
and
(* Checks precendence in order to add brackets *)
bracket_binop expr ?isRight:(isRight=false) subexpr =
  (* Add brackets if subexpression is lower precedence than main *)
  if (op_prec subexpr) < (op_prec expr) then
    add_brackets (string_of_expr subexpr)
  else
  (* If subexpr is on right side and equal precedence to main expression
   * we want to add brackets as well *)
    if (op_prec expr = op_prec subexpr) && isRight then
        add_brackets (string_of_expr subexpr)
    else
      string_of_expr subexpr
and
(* Convert unary operations to string
 * using bracket_unop to decide whether to add brackets or not 
 * depending on expression precedence *)
string_of_unop_expr unop expr =
  let unopConcat =
    match unop with
    | Op_minus -> String.concat ""
    | Op_not -> String.concat " "
  in
  unopConcat [string_of_unop unop; 
              bracket_unop (Eunop (unop,expr)) expr]
and
(* Adds brackets to expression following unary op if its precedence is lower than main expression *)
bracket_unop expr subexpr =
  if (op_prec subexpr) < (op_prec expr) then
    add_brackets (string_of_expr subexpr)
  else 
    string_of_expr subexpr

let string_of_rval rval =
  match rval with
  | Rexpr expr -> string_of_expr expr

let string_of_interval inter =
  let str = string_of_int
  in
  match inter with
  | Interval (low, high) -> String.concat ".." [str low;str high]

