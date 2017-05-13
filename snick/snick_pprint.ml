(* ----------------------------------------------------- | 
 * Pretty Printer for Snick language                     |
 * ----------------------------------------------------- |
 * Pretty prints a Snick program given the abstract      |
 * syntax tree built by the Snick parser and lexer       |
 * ----------------------------------------------------- | *)

open Snick_ast
open Format

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
  | Ebinop (_, Op_or, _, _)    -> 1
  | Ebinop (_, Op_and, _, _)   -> 2
  | Eunop  (Op_not, _, _)      -> 3
  | Ebinop (_, Op_eq, _, _)
  | Ebinop (_, Op_noteq, _, _)
  | Ebinop (_, Op_lt, _, _)
  | Ebinop (_, Op_lteq, _, _)
  | Ebinop (_, Op_gt, _, _)
  | Ebinop (_, Op_gteq, _, _)  -> 4
  | Ebinop (_, Op_add, _, _)
  | Ebinop (_, Op_sub, _, _)   -> 5
  | Ebinop (_, Op_mul, _, _)
  | Ebinop (_, Op_div, _, _)   -> 6
  | Eunop  ( Op_minus, _, _)   -> 7
  | _                       -> 8 
  (* Other exprs are all higher precedence *)

(* Print expressions *)
let rec pr_expr ppf expr =
  match expr with
  | Ebool (ebool, _) -> fprintf ppf "%s" (string_of_bool ebool)
  | Eint (eint, _) -> fprintf ppf "%s" (string_of_int eint)
  | Efloat (efloat, _) -> fprintf ppf "%s"  efloat
  | Estring (estring, _) -> fprintf ppf "%s" estring
  | Elval (elval, _) -> pr_lval ppf elval
  | Ebinop (lexpr, binop, rexpr, pos) -> pr_binop_expr ppf lexpr binop rexpr pos
  | Eunop (unop, expr, pos) -> pr_unop_expr ppf unop expr pos
and
(* Print lval here because lval and expr types are mutually recursive *)
pr_lval ppf lval =
  match lval with
  | LId (ident, _) -> fprintf ppf "%s" ident
  | LArray (ident, exprs, _) 
    -> fprintf ppf "%s[%a]" (ident) pr_comma_sep_exprs exprs
and
(* Print binary ops by
 * taking into account the precedence of left and right expressions
 * and adding brackets as needed to maintain the meaning of
 * the main expression stored in AST *)
pr_binop_expr ppf lexpr binop rexpr pos =
  let mainExpr = Ebinop (lexpr, binop, rexpr, pos)
  in
  fprintf ppf "%a %a %a" 
  bracket_binop_left mainExpr 
  pr_binop binop 
  bracket_binop_right mainExpr
and
(* Check if binary op left expr needs brackets 
 * and adds them if needed then prints out the expr *)
bracket_binop_left ppf expr =
  match expr with
  | Ebinop (lexpr, binop, rexpr, _) ->
    if (op_prec lexpr) < (op_prec expr) then
      fprintf ppf "(%a)" pr_expr lexpr
    else
      fprintf ppf "%a" pr_expr lexpr
  | _ -> () 
  (* Only match binop exprs *)
and
(* Check if binary op right expr needs brackets 
 * and adds them if needed then prints out the expr *)
bracket_binop_right ppf expr = 
  match expr with
  | Ebinop (lexpr, binop, rexpr, _) ->
    (* When right side is less than or equal in precedence to main expr
    * we need to add brackets *)
    if (op_prec rexpr) <= (op_prec expr) then
      fprintf ppf "(%a)" pr_expr rexpr
    else
      fprintf ppf "%a" pr_expr rexpr
  | _ -> ()
  (* Only matches binop exprs *)
and
(* Print unary ops
 * using bracket_unop to decide whether to add brackets or not 
 * depending on expression precedence *)
pr_unop_expr ppf unop expr pos =
    match unop with
    | Op_minus -> fprintf ppf "%a%a" 
                  pr_unop unop 
                  bracket_unop (Eunop (unop,expr, pos))
    | Op_not   -> fprintf ppf "%a %a" 
                  pr_unop unop 
                  bracket_unop (Eunop (unop,expr, pos))
and
(* Adds brackets to expression following unary op if 
 * its precedence is lower than main expression then prints *)
bracket_unop ppf expr =
  match expr with
  | Eunop (unop, subexpr, _) ->
    if (op_prec subexpr) < (op_prec expr) then
      fprintf ppf "(%a)" pr_expr subexpr
    else 
      fprintf ppf "%a" pr_expr subexpr
  | _ -> ()
  (* Only matches unop exprs *)
and
(* Prints a comma separated list of exprs using recursion *)
pr_comma_sep_exprs ppf exprs =
  match exprs with
  | [expr] -> fprintf ppf "%a" pr_expr expr
  | expr :: es -> fprintf ppf "%a, " pr_expr expr ; pr_comma_sep_exprs ppf es
  | [] -> ()

(* Prints rval *)
let pr_rval ppf rval =
  match rval with
  | Rexpr expr -> pr_expr ppf expr

(* Prints an interval of the form 1..8 *)
let pr_interval ppf inter =
  match inter with
  | Interval (low, high, _) ->
    fprintf ppf "%d..%d" low high
  
(* Print a comma separated list of intervals using recursion *)  
let rec pr_comma_sep_inters ppf inters =
  match inters with
  | [] -> ()
  | [inter] -> fprintf ppf "%a" pr_interval inter
  | inter :: is -> fprintf ppf "%a, " pr_interval inter 
                                    ; pr_comma_sep_inters ppf is

(* Prints snicktype *)
let pr_snicktype ppf stype =
  match stype with
  | Bool -> fprintf ppf "bool"
  | Int -> fprintf ppf "int"
  | Float -> fprintf ppf "float"

(* Prints passtype *)
let pr_passtype ppf ptype =
  match ptype with
  | Val -> fprintf ppf "val"
  | Ref -> fprintf ppf "ref"

(* Prints declaration by calling other print functions *)
let pr_decl ppf decl =
  match decl with
  | RegDecl (id, stype, _) 
    -> fprintf ppf "%a %s;" pr_snicktype stype id
  | ArrayDecl (id, stype, inters, _)
    -> fprintf ppf "%a %s[%a];" pr_snicktype stype id pr_comma_sep_inters inters

(* Prints list of decls by recursively calling pr_decl *)
let rec pr_decls ppf decls =
  match decls with
  | [] -> ()
  (* Line break after last decl *)
  | [decl] -> fprintf ppf "%a@;" pr_decl decl
  (* Opens and closes vbox *)
  | decl :: ds -> fprintf ppf "@[<v>%a@;@]" pr_decl decl ; pr_decls ppf ds

(* Prints statement by calling the relative function for printing *)
let rec pr_stmt ppf stmt =
  match stmt with
  | Assign (lval, rval, _) 
    -> fprintf ppf "%a := %a;" pr_lval lval pr_rval rval
  | Read lval -> fprintf ppf "read %a;" pr_lval lval
  | Write expr -> fprintf ppf "write %a;" pr_expr expr
  | Ifthen (expr, stmts) -> pr_ifThen ppf expr stmts
  | IfthenElse (expr, thenStmts, elseStmts) 
    -> pr_ifThenElse ppf expr thenStmts elseStmts
  | WhileDo (expr, stmts) -> pr_whileDo ppf expr stmts
  | ProcCall (id, exprs, _) 
    -> fprintf ppf "%s(%a);" id pr_comma_sep_exprs exprs
and
(* Print statements by recursively calling pr_stmt *)
pr_stmts ppf stmts = 
  match stmts with
  | [] -> ()
  | [stmt] -> fprintf ppf "%a"  pr_stmt stmt
  | stmt :: ss -> fprintf ppf "@[<v>%a@;@]" pr_stmt stmt; pr_stmts ppf ss
and
(* Print ifThen stmt *)
pr_ifThen ppf expr stmts =
  fprintf ppf
  "@[<v>if %a then@;<0 4>%a@;fi@]" 
  pr_expr expr pr_stmts (List.rev stmts)
and
(* Print ifThenElse stmt *)
pr_ifThenElse ppf expr thenStmts elseStmts =
  fprintf ppf
  "@[<v>if %a then@;<0 4>%a@;else@;<0 4>%a@;fi@]"
  pr_expr expr
  pr_stmts (List.rev thenStmts)
  pr_stmts (List.rev elseStmts)
and
(* Print whileDo stmt *)
pr_whileDo ppf expr stmts =
  fprintf ppf
  "@[<v>while %a do@;<0 4>%a@;od@]"
  pr_expr expr
  pr_stmts (List.rev stmts)
  (* Reversing statement list within composite statements 
   * because of doubly recursive statements flipping the order again*)

(* Print process argument *)
let pr_arg ppf (ptype, stype, id, _) =
  fprintf ppf "%a %a %s" pr_passtype ptype pr_snicktype stype id

(* Print comma separated arguments by
 * recursively calling pr_arg *)
let rec pr_comma_sep_args ppf args =
  match args with
  | [] -> ()
  | [arg] -> fprintf ppf "%a" pr_arg arg
  | arg :: ars -> fprintf ppf "%a, " pr_arg arg ; pr_comma_sep_args ppf ars

(* Print arguments in brackets
 * separating the case where no args
 * and some args *)
let pr_args ppf args =
  if args = [] then
    fprintf ppf "()"
    (* Empty arguments case handled here
     * Can't be handled inside recursion *)
  else
    fprintf ppf "(%a)" pr_comma_sep_args args

(* Print the process body *)
let pr_proc_body ppf (decls, stmts) =
  fprintf ppf
  ("@[<v>%a@;%a@]")
  pr_decls decls
  pr_stmts stmts

(* Print the full process flushing pretty
 * printer at the end of each process
 * because each process is separate *)
let pr_proc ppf (id, args, proc_body, _) =
  fprintf ppf
  "@[<v>proc %s %a@;<0 4>%a@;end@]@."
  id pr_args args pr_proc_body proc_body

(* Print all processes by calling pr_proc *)
let rec pr_proc_list ppf procs =
  match procs with
  | [] -> ()
  (* Double line break to get blank line at end *)
  | [proc] -> fprintf ppf "%a@;@;" pr_proc proc
  (* Open and close box for each process *)
  | proc :: ps -> fprintf ppf  "@[<v>%a@]@." pr_proc proc ; pr_proc_list ppf ps

(* Prints the program *)
let rec print_program ppf prog =
  let procs = prog.procs
  in
  pr_proc_list ppf procs;
