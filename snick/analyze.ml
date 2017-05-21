(*===== Snick Semantics =====*)

module AST = Snick_ast
module ST = Symbol

(*---- Exceptions ----*)

(* Generic semantic error definition *)
exception Semantic_error of string * AST.pos

(* Some random error *)
exception Undefined_error

(* Possible errors that need to be caught during compilation *)
exception Non_Scalar_Expression of string * AST.pos
exception Non_Boolean_Condition of string * AST.pos
exception Incorrect_Number_of_Args of string * AST.pos
exception Reference_Error of string * AST.pos
exception Variable_Name_Conflict of string * AST.pos
exception Type_Error of string * AST.pos
exception Main_Has_Args
exception Missing_Main

(*---- Helper Functions ----*)

(* Function used to check whether types are same *)
let same_type tsym1 tsym2 =
  match (tsym1, tsym2) with
  | (ST.Tsym AST.Bool, ST.Tsym AST.Bool)
  | (ST.Tsym AST.Int, ST.Tsym AST.Int)
  | (ST.Tsym AST.Float, ST.Tsym AST.Float) -> true
  | _ -> false

(* Function used to check if types can be converted *)
let is_convert_eq_type tsym1 tsym2 =
  match (tsym1, tsym2) with
  | (ST.Tsym AST.Int, ST.Tsym AST.Float)
  | (ST.Tsym AST.Float, ST.Tsym AST.Int) -> true
  | _ -> false

let get_expr_pos expr =
  match expr with
  | AST.Ebool (_, pos) -> pos
  | AST.Eint (_, pos) -> pos
  | AST.Efloat (_, pos) -> pos
  | AST.Elval (_, pos) -> pos
  | AST.Ebinop (_, _, _, pos) -> pos
  | AST.Eunop (_, _, pos) -> pos

let rec expr_type stbl proc_id expr =
  match expr with
  | AST.Ebool _ -> ST.Tsym AST.Bool
  | AST.Eint _ -> ST.Tsym AST.Int
  | AST.Efloat _ -> ST.Tsym AST.Float
  | AST.Elval (lval, pos) -> ST.get_lval_type stbl proc_id lval pos
  | AST.Ebinop (lexpr, binop, rexpr, pos) -> 
      binop_type stbl proc_id lexpr binop rexpr pos
  | AST.Eunop (unop, subexpr, _) -> unop_type stbl proc_id unop subexpr
and unop_type stbl proc_id unop subexpr =
  match (unop, subexpr) with
  | (AST.Op_not, _) -> ST.Tsym AST.Bool
  | (_, sexpr) -> expr_type stbl proc_id sexpr
and binop_type stbl proc_id lexpr binop rexpr pos =
  match (lexpr, binop, rexpr) with
  | (_, AST.Op_eq, _)
  | (_, AST.Op_lt, _)
  | (_, AST.Op_gt, _)
  | (_, AST.Op_noteq, _)
  | (_, AST.Op_gteq, _)
  | (_, AST.Op_lteq, _)
  | (_, AST.Op_and, _)
  | (_, AST.Op_or, _) -> ST.Tsym AST.Bool
  | (lexpr, AST.Op_add, rexpr)
  | (lexpr, AST.Op_sub, rexpr)
  | (lexpr, AST.Op_mul, rexpr)
  | (lexpr, AST.Op_div, rexpr) ->
      resolve_arithmetic_type (expr_type stbl proc_id lexpr) 
                        (expr_type stbl proc_id rexpr) pos
and resolve_arithmetic_type lt rt pos =
  match (lt, rt) with
  | (ST.Tsym AST.Int, ST.Tsym AST.Float)
  | (ST.Tsym AST.Float, ST.Tsym AST.Int)
  | (ST.Tsym AST.Float, ST.Tsym AST.Float) -> ST.Tsym AST.Float
  | (ST.Tsym AST.Int, ST.Tsym AST.Int) -> ST.Tsym AST.Int
  | _ -> raise (Type_Error("Non-Arithmetic type", pos))

let valid_binop_operand binop op_type =
  let is_num t = 
    same_type (ST.Tsym AST.Int) t || same_type (ST.Tsym AST.Float) t in
  let is_bool t =
    same_type (ST.Tsym AST.Bool) t in
  let any t = is_num t || is_bool t in
  match binop with
  | AST.Op_eq
  | AST.Op_noteq -> any op_type
  | AST.Op_lt
  | AST.Op_gt
  | AST.Op_gteq
  | AST.Op_lteq
  | AST.Op_add
  | AST.Op_sub
  | AST.Op_mul
  | AST.Op_div -> is_num op_type
  | AST.Op_and
  | AST.Op_or -> is_bool op_type

let valid_unop_operand unop op_type =
  let is_num t = 
    same_type (ST.Tsym AST.Int) t || same_type (ST.Tsym AST.Float) t in
  let is_bool t =
    same_type (ST.Tsym AST.Bool) t in
  match unop with
  | AST.Op_minus -> is_num op_type
  | AST.Op_not -> is_bool op_type



(*---- Semantic Checks ----*)
let check_var_defined stbl proc_id id pos =
  let ptbl = Hashtbl.find stbl.ST.sprocs id in
  if Hashtbl.mem ptbl.ST.proc_stbl id then
    ()
  else
    raise (ST.Undefined_variable(id, pos))

let check_decl_name stbl proc_id id pos =
  let scope = ST.get_var_scope stbl proc_id id pos in
  match scope with
  | ST.SDecl -> ()
  | ST.SRefParam | ST.SValParam ->
      raise (Variable_Name_Conflict(id, pos))

let check_proc_defined stbl proc_id pos =
  if Hashtbl.mem stbl.ST.sprocs proc_id then
    ()
  else
    raise (ST.Undefined_process(proc_id, pos))

let check_proc_call_args stbl proc_id args pos =
  let params = ST.get_args stbl proc_id pos in
  if List.length params = List.length args then
    ()
  else
    raise (Incorrect_Number_of_Args(proc_id, pos))

let check_main stbl =
  let ptbl = stbl.ST.sprocs in
  if Hashtbl.mem ptbl "main" then
    let main = Hashtbl.find ptbl "main" in
    if List.length main.ST.proc_args = 0 then
      ()
    else
      raise Main_Has_Args
  else
    raise Missing_Main

let check_lval stbl proc_id lval =
  match lval with
  | AST.LId (id, pos) -> check_var_defined stbl proc_id id pos
  | _ -> () (* Array check here *)

let check_pass_type stbl caller callee id arg pos =
  let psym = ST.get_var_sym stbl callee id pos in
  let (_, scope, _, _) = psym in
  match scope with
  | ST.SDecl -> raise Undefined_error
  | ST.SValParam -> ()
  | ST.SRefParam ->
      match arg with
      | AST.Elval _ -> ()
      | _ ->
        let pos = get_expr_pos arg in
        raise (Reference_Error("Not a valid reference", pos))


