open Snack_ast
open Format

let ppf = Format.std_formatter
(* Print Binary operators *)
let pr_binop ppf binop =
  match binop with
  | Op_add  -> fprintf ppf "+"
  | Op_sub  -> fprintf ppf "-"
  | Op_mul  -> fprintf ppf "*"
  | Op_div  -> fprintf ppf "/"
  | Op_and  -> fprintf ppf "and"
  | Op_or   -> fprintf ppf "or"
  | Op_eq   -> fprintf ppf "="
  | Op_noteq  -> fprintf ppf "!="
  | Op_lt   -> fprintf ppf "<"
  | Op_lteq  -> fprintf ppf "<="
  | Op_gt   -> fprintf ppf ">"
  | Op_gteq  -> fprintf ppf ">="

(* Print Unary operators *)
let pr_unop ppf unop =
  match unop with
  | Op_minus -> fprintf ppf "-"
  | Op_not -> fprintf ppf "not"

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
  (* Other exprs are all higher precedence *)

let rec pr_comma_sep_list ppf pr_fun pr_list =
  match pr_list with
  | [] -> ()
  | [l] -> pr_fun ppf l
  | l :: ls -> pr_fun ppf l; fprintf ppf ", " ; pr_comma_sep_list ppf pr_fun ls

(* Convert expression to string *)
let rec pr_expr ppf expr =
  match expr with
  | Ebool ebool -> fprintf ppf "%s" (string_of_bool ebool)
  | Eint eint -> fprintf ppf "%s" (string_of_int eint)
  | Efloat efloat -> fprintf ppf "%s" (string_of_float efloat)
  | Estring estring -> fprintf ppf "%s" estring
  | Elval elval -> pr_lval ppf elval
  | Ebinop (lexpr, binop, rexpr) -> pr_binop_expr ppf lexpr binop rexpr
  | Eunop (unop, expr) -> pr_unop_expr ppf unop expr
and
(* Convert lval to string, using and here because lval and expr types are mutually recursive *)
pr_lval ppf lval =
  match lval with
  | LId ident -> fprintf ppf "%s" ident
  | LArray (ident, exprs) 
    -> fprintf ppf "%s[%a]" (ident) (pr_comma_sep_list ppf pr_expr exprs)
  (* Concat ident to front of a square bracketed list of comma separated expressions converted to string *)
and
(* Convert binary operations to string by
 * taking into account the precedence of left and right expressions
 * and adding brackets as needed to maintain the meaning of
 * the main expression stored in AST *)
pr_binop_expr ppf lexpr binop rexpr =
  let mainExpr = Ebinop (lexpr, binop, rexpr)
  in
  fprintf ppf "%a %a %a" 
  (bracket_binop ppf mainExpr lexpr)
  (pr_binop ppf binop)
  (bracket_binop ppf mainExpr rexpr ~isRight:true)
  (*String.concat " " [bracket_binop pr mainExpr lexpr;
                string_of_binop pr binop; 
                bracket_binop pr mainExpr rexpr ~isRight:true]*)
and
(* Checks precendence in order to add brackets *)
bracket_binop ppf expr ?isRight:(isRight=false) subexpr =
  (* Add brackets if subexpression is lower precedence than main *)
  if (op_prec subexpr) < (op_prec expr) then
    fprintf ppf "(%a)" (pr_expr ppf subexpr)
  else
  (* If subexpr is on right side and equal precedence to main expression
   * we want to add brackets as well *)
    if (op_prec expr = op_prec subexpr) && isRight then
      fprintf ppf "(%a)" (pr_expr ppf subexpr)
    else
      pr_expr ppf subexpr
and
(* Convert unary operations to string
 * using bracket_unop to decide whether to add brackets or not 
 * depending on expression precedence *)
pr_unop_expr ppf unop expr =
    match unop with
    | Op_minus -> fprintf ppf "%a%a" (pr_unop unop) (bracket_unop ppf (Eunop (unop,expr)) expr)
    | Op_not -> fprintf ppf "%a %a" (pr_unop unop) (bracket_unop ppf (Eunop (unop,expr)) expr)
and
(* Adds brackets to expression following unary op if its precedence is lower than main expression *)
bracket_unop ppf expr subexpr =
  if (op_prec subexpr) < (op_prec expr) then
    fprintf ppf "(%a)" (pr_expr ppf subexpr)
  else 
   fprintf ppf "%a" (pr_expr ppf subexpr)

let pr_rval ppf rval =
  match rval with
  | Rexpr expr -> pr_expr ppf expr

let pr_interval ppf inter =
  match inter with
  | Interval (low, high) ->
    fprintf ppf "%d..%d" low high 

let pr_snacktype ppf stype =
  match stype with
  | Bool -> fprintf ppf "bool"
  | Int -> fprintf ppf "int"
  | Float -> fprintf ppf "float"

let pr_passtype ppf ptype =
  match ptype with
  | Val -> fprintf ppf "val"
  | Ref -> fprintf ppf "ref"

let pr_decl ppf decl =
  match decl with
  | RegDecl (id, stype) 
    -> fprintf ppf "%a %s;" (pr_snacktype ppf stype) id
  | ArrayDecl (id, stype, inters)
    -> fprintf ppf "%a %s[%a];" (pr_snacktype ppf stype) id (pr_comma_sep_list ppf pr_interval inters)

let rec pr_decls ppf decls =
  match decls with
  | [decl] -> fprintf ppf "@[<v>%a@;" (pr_decl ppf decl)
  | decl :: ds -> fprintf ppf "%a@;" (pr_decl ppf decl) ; pr_decls ppf ds
  | [] -> fprintf ppf "@]"
  (*(String.concat
  (String.concat "@;" (List.map (string_of_decl pr) decls))
  ["@[<v>";"@]"])*)

let rec pr_stmts ppf stmts =
  let pr_stmt ppf stmt =
    match stmt with
    | Assign (lval, rval) 
      -> fprintf ppf "%a := %a;" (pr_lval ppf lval) (pr_rval ppf rval)
      (*"(String.concat " " 
        [string_of_lval pr lval;":=";string_of_rval pr rval] ^
        ";")"*)
    | Read lval -> fprintf ppf "read %a;" (pr_lval ppf lval)
    (*pr ("read " ^ string_of_lval pr lval ^ ";")*)
    | Write expr -> fprintf ppf "write %a;" (pr_expr ppf expr)
    (*pr ("write " ^ string_of_expr pr expr ^ ";")*)
    | Ifthen (expr, stmts) -> pr_ifThen ppf expr stmts
    | IfthenElse (expr, thenStmts, elseStmts) 
      -> pr_ifThenElse ppf expr thenStmts elseStmts
    | WhileDo (expr, stmts) -> pr_whileDo ppf expr stmts
    | ProcCall (id, exprs) 
      -> fprintf ppf "%s (%a)" id (pr_comma_sep_list ppf pr_expr exprs)
      (*(id ^ add_brackets (String.concat ", " 
        (List.map (string_of_expr pr) exprs)) ^ ";")*)
  in
  match stmts with
  | [stmt] -> fprintf ppf "@[<v>%a@;" (pr_stmt ppf stmt)
  | stmt :: ss -> fprintf ppf "%a@;" (pr_stmt ppf stmt)
  | [] -> fprintf ppf "@]"
  (*(String.concat 
  (String.concat "@;" (List.map (string_of_stmt pr) stmts))
  ["@[<v>";"@]"]) *)
and
pr_ifThen ppf expr stmts =
  fprintf ppf
  "@[<v>if %a then@;<0 4>%a@;fi@]" 
  (pr_expr ppf expr) (pr_stmts ppf stmts)
and
pr_ifThenElse ppf expr thenStmts elseStmts =
  fprintf ppf
  "@[<v>if %a then@;<0 4>%a@;else@;<0 4>%a@;fi@]"
  (pr_expr ppf expr)
  (pr_stmts ppf thenStmts) 
  (pr_stmts ppf elseStmts)
and
pr_whileDo ppf expr stmts =
  fprintf ppf
  "@[<v>while %a do@;<0 4>%a@;od@]"
  (pr_expr ppf expr)
  (pr_stmts ppf stmts)

let pr_arg ppf (ptype, stype, id) =
  fprintf ppf "%a %a %s" (pr_passtype ppf ptype) (pr_snacktype ppf stype) id  
  (*(String.concat " " 
  [string_of_passtype pr ptype;
  string_of_snacktype pr stype;
  id])*)

let pr_args ppf args =
  fprintf ppf "(%a)" (pr_comma_sep_list ppf pr_arg args)
  (*pr (add_brackets (String.concat ", "
               (List.map (string_of_arg pr) args)))*)

let pr_proc_body ppf (decls, stmts) =
  pr
  ("@[<v>%a@;@;%a@]")
  (pr_decls ppf decls)
  (pr_stmts ppf stmts)

let pr_proc ppf (id, args, proc_body) =
  fprintf ppf
  "@[<v>proc %s %a@;<0 4>%a@;end"
  id
  (pr_args ppf args) 
  (pr_proc_body ppf proc_body)

let print_program ppf prog =
  let procs = prog.procs
  in
  match procs with
  | [proc] -> fprintf ppf "@[<v>%a@;@;" (pr_proc ppf proc)
  | proc :: ps -> fprintf ppf "%a@;@;" (pr_proc ppf proc)
  | [] -> fprintf ppf "@]@."
  (*pr (String.concat 
  (String.concat "@;@;" (List.map (string_of_proc pr) procs))
  ["@[<v>";"@]@."])*)

(*let print_program fmt prog = 
  let pr = fprintf fmt
  in
  pr_program pr prog
  (*fprintf fmt "@[<v>--@;<0 2>@[<v>(---%(fmt%)@;--)@]@;@;--@]@."
  (format_of_string ";@;")*)
  (*fprintf fmt "@[<v>proc x (val int y, ref float z)@;<0 4>@[<v>@[<v>int y;@;float z;@]@;@;@[<v>y := z;@]@]@;end@]@."*)*)