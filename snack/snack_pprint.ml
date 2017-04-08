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
    -> fprintf ppf "%s[%a]" (ident) pr_comma_sep_exprs exprs
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
  bracket_binop_left mainExpr 
  pr_binop binop 
  bracket_binop_right mainExpr
and
bracket_binop_left ppf (Ebinop (lexpr, binop, rexpr)) =
  let expr = Ebinop (lexpr, binop, rexpr)
  in
  if (op_prec lexpr) < (op_prec expr) then
    fprintf ppf "(%a)" pr_expr lexpr
  else
    fprintf ppf "%a" pr_expr lexpr
and
bracket_binop_right ppf (Ebinop (lexpr, binop, rexpr)) = 
  let expr = Ebinop (lexpr, binop, rexpr)
  in
  (* When right side is less than or equal in precedence to main expr
   * we need to add brackets *)
  if (op_prec rexpr) <= (op_prec expr) then
    fprintf ppf "(%a)" pr_expr rexpr
  else
    fprintf ppf "%a" pr_expr rexpr
and
(* Convert unary operations to string
 * using bracket_unop to decide whether to add brackets or not 
 * depending on expression precedence *)
pr_unop_expr ppf unop expr =
    match unop with
    | Op_minus -> fprintf ppf "%a%a" 
                  pr_unop unop 
                  bracket_unop (Eunop (unop,expr))
    | Op_not   -> fprintf ppf "%a %a" 
                  pr_unop unop 
                  bracket_unop (Eunop (unop,expr))
and
(* Adds brackets to expression following unary op if its precedence is lower than main expression *)
bracket_unop ppf (Eunop (unop, expr)) =
  let mainExpr = Eunop (unop, expr)
  in
  if (op_prec expr) < (op_prec mainExpr) then
    fprintf ppf "(%a)" pr_expr expr
  else 
    fprintf ppf "%a" pr_expr expr
and
pr_comma_sep_exprs ppf exprs =
  match exprs with
  | [expr] -> fprintf ppf "%a" pr_expr expr
  | expr :: es -> fprintf ppf "%a, " pr_expr expr ; pr_comma_sep_exprs ppf es
  | [] -> ()

let pr_rval ppf rval =
  match rval with
  | Rexpr expr -> pr_expr ppf expr

let pr_interval ppf inter =
  match inter with
  | Interval (low, high) ->
    fprintf ppf "%d..%d" low high
  
let rec pr_comma_sep_inters ppf inters =
  match inters with
  | [] -> ()
  | [inter] -> fprintf ppf "%a" pr_interval inter
  | inter :: is -> fprintf ppf "%a, " pr_interval inter ; pr_comma_sep_inters ppf is

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
    -> fprintf ppf "%a %s;" pr_snacktype stype id
  | ArrayDecl (id, stype, inters)
    -> fprintf ppf "%a %s[%a];" pr_snacktype stype id pr_comma_sep_inters inters

let rec pr_decls ppf decls =
  match decls with
  | [] -> fprintf ppf "%(fmt%)" "@]"  
  | [decl] -> fprintf ppf "@[<v>%a@;" pr_decl decl
  | decl :: ds -> fprintf ppf "%a@;" pr_decl decl ; pr_decls ppf ds

let rec pr_stmt ppf stmt =
  match stmt with
  | Assign (lval, rval) 
    -> fprintf ppf "%a := %a;" pr_lval lval pr_rval rval
  | Read lval -> fprintf ppf "read %a;" pr_lval lval
  (*pr ("read " ^ string_of_lval pr lval ^ ";")*)
  | Write expr -> fprintf ppf "write %a;" pr_expr expr
  (*pr ("write " ^ string_of_expr pr expr ^ ";")*)
  | Ifthen (expr, stmts) -> pr_ifThen ppf expr stmts
  | IfthenElse (expr, thenStmts, elseStmts) 
    -> pr_ifThenElse ppf expr thenStmts elseStmts
  | WhileDo (expr, stmts) -> pr_whileDo ppf expr stmts
  | ProcCall (id, exprs) 
    -> fprintf ppf "%s(%a)" id pr_comma_sep_exprs exprs
    (*(id ^ add_brackets (String.concat ", " 
      (List.map (string_of_expr pr) exprs)) ^ ";")*)
and
pr_stmts ppf stmts = 
  match stmts with
  | [] -> fprintf ppf "%(fmt%)" "@]"
  | [stmt] -> fprintf ppf "@[<v>%a@;" pr_stmt stmt
  | stmt :: ss -> fprintf ppf "%a@;" pr_stmt stmt; pr_stmts ppf ss
and
pr_ifThen ppf expr stmts =
  fprintf ppf
  "@[<v>if %a then@;<0 4>%afi@]" 
  pr_expr expr pr_stmts (List.rev stmts)
and
pr_ifThenElse ppf expr thenStmts elseStmts =
  fprintf ppf
  "@[<v>if %a then@;<0 4>%aelse@;<0 4>%afi@]"
  pr_expr expr
  pr_stmts (List.rev thenStmts)
  pr_stmts (List.rev elseStmts)
and
pr_whileDo ppf expr stmts =
  fprintf ppf
  "@[<v>while %a do@;<0 4>%aod@]"
  pr_expr expr
  pr_stmts (List.rev stmts)
  (* Reversing statement list within composite statements 
   * because of doubly recursive statements flipping the order again*)

let pr_arg ppf (ptype, stype, id) =
  fprintf ppf "%a %a %s" pr_passtype ptype pr_snacktype stype id

let rec pr_comma_sep_args ppf args =
  match args with
  | [] -> ()
  | [arg] -> fprintf ppf "%a" pr_arg arg
  | arg :: ars -> fprintf ppf "%a, " pr_arg arg ; pr_comma_sep_args ppf ars

let pr_args ppf args =
  if args = [] then
    fprintf ppf "()"
    (* Empty arguments case handled here
     * Can't be handled inside recursion *)
  else
    fprintf ppf "(%a)" pr_comma_sep_args args

let pr_proc_body ppf (decls, stmts) =
  fprintf ppf
  ("@[<v>%a@;%a@]")
  pr_decls decls
  pr_stmts stmts

let pr_proc ppf (id, args, proc_body) =
  fprintf ppf
  "@[<v>proc %s %a@;<0 4>%a@;end@]"
  id pr_args args pr_proc_body proc_body

let rec pr_proc_list ppf procs =
  match procs with
  | [] -> fprintf ppf "%(fmt%)" "@]@."
  | [proc] -> fprintf ppf "@[<v>%a@;@;" pr_proc proc
  | proc :: ps -> fprintf ppf "%a@;@;" pr_proc proc ; pr_proc_list ppf ps

let rec print_program ppf prog =
  let procs = prog.procs
  in
  pr_proc_list ppf procs;
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