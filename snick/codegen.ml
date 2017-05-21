module AST = Snick_ast
module ST = Symbol
module AN = Analyze

(*----- Data Structure -----*)

type reg = Reg of int (* A register *)
type stack_slot = StackSlot of int (* Slot in Stack Frame *)
type label = Label of string (* Block label *)
type scope = Val | Ref (* Variable scope *)

(* Builtin Brill functions *)
type builtin =
  | ReadInt
  | ReadReal
  | ReadBool
  | PrintInt
  | PrintReal
  | PrintBool
  | PrintString

(* Brill Instructions *)
type instruction =
  (* Stack Manipulation Instructions *)
  | PushStackFrame of int
  | PopStackFrame of int
  (* Register/Stack Slot Instructions *)
  | Load of reg * stack_slot
  | Store of stack_slot * reg
  | LoadAddress of reg * stack_slot
  | LoadIndirect of reg * reg
  | StoreIndirect of reg * reg
  (* Constant -> Reg Instructions *)
  | IntConst of reg * int
  | RealConst of reg * float
  | StringConst of reg * string
  (* Arithmetic Operations *)
  | AddInt of reg * reg * reg
  | AddReal of reg * reg * reg
  | AddOffset of reg * reg * reg
  | SubInt of reg * reg * reg
  | SubReal of reg * reg * reg
  | SubOffset of reg * reg * reg
  | MulInt of reg * reg * reg
  | MulReal of reg * reg * reg
  | DivInt of reg * reg * reg
  | DivReal of reg * reg * reg
  (* Comparison Operations *)
  | CmpEqInt of reg * reg * reg
  | CmpNeInt of reg * reg * reg
  | CmpGtInt of reg * reg * reg
  | CmpGeInt of reg * reg * reg
  | CmpLtInt of reg * reg * reg
  | CmpLeInt of reg * reg * reg
  | CmpEqReal of reg * reg * reg
  | CmpNeReal of reg * reg * reg
  | CmpGtReal of reg * reg * reg
  | CmpGeReal of reg * reg * reg
  | CmpLtReal of reg * reg * reg
  | CmpLeReal of reg * reg * reg
  (* Boolean Operations *)
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Not of reg * reg
  (* Conversion of Int->Float *)
  | IntToReal of reg * reg
  (* Move *)
  | Move of reg * reg
  (* Branch *)
  | BranchOnTrue of reg * label
  | BranchOnFalse of reg * label
  | BranchUncond of label
  (* Calls *)
  | Call of label
  | CallBuiltin of builtin
  (* Return *)
  | Return
  (* Debug functions *)
  | DebugReg of reg
  | DebugSlot of stack_slot
  | DebugStack
  (* Stop Emulator *)
  | Halt

type code = instruction list

(*----- Exceptions -----*)
exception Unimplemented of string
exception Uncaught_Semantic_Error of string

(*----- Code Generation -----*)

(*---- Declarations ----*)
let gen_reg_decl_code frame_size t =
  match t with
  | AST.Int -> [Store (StackSlot frame_size, Reg 0); IntConst(Reg 0, 0)]
  | AST.Float -> [Store (StackSlot frame_size, Reg 0); RealConst(Reg 0, 0.0)]
  | AST.Bool -> [Store (StackSlot frame_size, Reg 0); IntConst(Reg 0, 0)]

let get_decl_code stbl proc (frame_size, code) decl = 
  match decl with
  | AST.RegDecl (id, rtype, pos) ->
      let new_slot = ST.set_id_slot stbl proc id frame_size pos in
      let code = gen_reg_decl_code frame_size rtype in
      (new_slot, code)
  | AST.ArrayDecl _ -> (frame_size, []) (* Array code here *)

(*---- Type Resolution ----*)
let get_pass_type scope =
  match scope with
  | ST.SDecl
  | ST.SValParam -> Val
  | ST.SRefParam -> Ref

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
  | (ST.Tsym AST.Float, ST.Tsym AST.Float) -> ST.Tsym AST.Float
  | (ST.Tsym AST.Int, ST.Tsym AST.Int) -> ST.Tsym AST.Int
  | _ -> raise (AN.Type_Error("Arithmetic type doesn't match", pos))

(*---- IO Read ----*)

(* Generate read code *)
let gen_read stbl proc_id lval =
  match lval with
  | AST.LId (id, pos) -> 
      let ST.Tsym t = ST.get_lval_type stbl proc_id lval pos in
      let read =
        (match t with
        | AST.Int -> ReadInt
        | AST.Float -> ReadReal
        | AST.Bool -> ReadBool)
      in
      let slot_num = ST.get_id_slot stbl proc_id id pos in
      (match ST.get_var_scope stbl proc_id id pos with
      | ST.SDecl
      | ST.SValParam -> [Store (StackSlot slot_num, Reg 0);
                         CallBuiltin read]
      | ST.SRefParam -> [StoreIndirect (Reg 1, Reg 0);
                         Load (Reg 1, StackSlot slot_num);
                         CallBuiltin read])
  | AST.LArray _ -> []

(*---- Expression Evaluation ----*)

let gen_int_const load_reg i =
  [IntConst (load_reg, i)]

let gen_real_const load_reg n =
  [RealConst (load_reg, n)]

let gen_bool_const load_reg b =
  match b with
  | true -> [IntConst (load_reg, 1)]
  | false -> [IntConst (load_reg, 0)]

let gen_lval stbl proc_id load_reg lval =
  let (scope, slot_num) =
    match lval with
    | AST.LArray _ -> (ST.SRefParam, 0)
    | AST.LId (id, pos) ->
      (ST.get_lval_scope stbl proc_id lval pos, 
       ST.get_id_slot stbl proc_id id pos)
  in
  match scope with
  | ST.SDecl
  | ST.SValParam -> [Load (load_reg, StackSlot slot_num)]
  | ST.SRefParam -> [LoadIndirect (load_reg, load_reg); 
                     Load (load_reg, StackSlot slot_num)]

let rec gen_unop_expr stbl proc_id load_reg unop expr =
  let subexpr_code = gen_expr stbl proc_id load_reg expr in
  let ST.Tsym subexpr_type = expr_type stbl proc_id expr in
  let (Reg r) = load_reg in
  let unop_code =
    match (unop, subexpr_type) with
    | (AST.Op_minus, AST.Int) ->
        [MulInt (Reg r, Reg r, Reg (r+1));
        IntConst (Reg (r+1), -1)]
    | (AST.Op_minus, AST.Float) ->
        [MulReal (Reg r, Reg r, Reg (r+1));
        RealConst (Reg (r+1), -1.0)]
    | (AST.Op_not, AST.Bool) -> [Not (Reg r, Reg r)]
    | _ -> raise 
        (Uncaught_Semantic_Error "Boolean op requires boolean operands")
  in
  unop_code @ subexpr_code
and
gen_binop_expr stbl proc_id load_reg binop lexpr rexpr =
  let (Reg r) = load_reg in
  let lexpr_code = gen_expr stbl proc_id load_reg lexpr in
  let rexpr_code = gen_expr stbl proc_id load_reg rexpr in
  let ST.Tsym lexpr_t = expr_type stbl proc_id lexpr in
  let ST.Tsym rexpr_t = expr_type stbl proc_id rexpr in
  let con_l = IntToReal (Reg r, Reg r) in
  let con_r = IntToReal (Reg (r+1), Reg (r+1)) in
  let binop_code =
    match (lexpr_t, binop, rexpr_t) with
    (* Arithmetic Conversion operations *)
    | (AST.Int, AST.Op_add, AST.Float) -> [AddReal (Reg r, Reg r, Reg (r+1));
                                           con_l]
    | (AST.Float, AST.Op_add, AST.Int) -> [AddReal (Reg r, Reg r, Reg (r+1));
                                           con_r]
    | (AST.Int, AST.Op_sub, AST.Float) -> [SubReal (Reg r, Reg r, Reg (r+1));
                                           con_l]
    | (AST.Float, AST.Op_sub, AST.Int) -> [SubReal (Reg r, Reg r, Reg (r+1));
                                           con_r]
    | (AST.Int, AST.Op_mul, AST.Float) -> [MulReal (Reg r, Reg r, Reg (r+1));
                                           con_l]
    | (AST.Float, AST.Op_mul, AST.Int) -> [MulReal (Reg r, Reg r, Reg(r+1));
                                           con_r]
    | (AST.Int, AST.Op_div, AST.Float) -> [DivReal (Reg r, Reg r, Reg (r+1));
                                           con_l]
    | (AST.Float, AST.Op_div, AST.Int) -> [DivReal (Reg r, Reg r, Reg (r+1));
                                           con_r]
    (* Comparison Conversion operations *)
    | (AST.Int, AST.Op_gt, AST.Float) -> [CmpGtReal (Reg r, Reg r, Reg (r+1));
                                          con_l]
    | (AST.Float, AST.Op_gt, AST.Int) -> [CmpGtReal (Reg r, Reg r, Reg (r+1));
                                          con_r]
    | (AST.Int, AST.Op_gteq, AST.Float) -> [CmpGeReal (Reg r, Reg r, Reg (r+1));
                                            con_l]
    | (AST.Float, AST.Op_gteq, AST.Int) -> [CmpGeReal (Reg r, Reg r, Reg (r+1));
                                            con_r]
    | (AST.Int, AST.Op_lt, AST.Float) -> [CmpLtReal (Reg r, Reg r, Reg (r+1));
                                          con_l]
    | (AST.Float, AST.Op_lt, AST.Int) -> [CmpLtReal (Reg r, Reg r, Reg (r+1));
                                          con_r]
    | (AST.Int, AST.Op_lteq, AST.Float) -> [CmpLeReal (Reg r, Reg r, Reg (r+1));
                                            con_l]
    | (AST.Float, AST.Op_lteq, AST.Int) -> [CmpLeReal (Reg r, Reg r, Reg (r+1));
                                            con_r]
    (* Boolean Ops *)
    | (AST.Bool, AST.Op_and, AST.Bool) -> [And (Reg r, Reg r, Reg(r+1))]
    | (_, AST.Op_and, _) -> raise (Uncaught_Semantic_Error "Boolean op AND")
    | (AST.Bool, AST.Op_or, AST.Bool) -> [Or (Reg r, Reg r, Reg(r+1))]
    | (_, AST.Op_or, _) -> raise (Uncaught_Semantic_Error "Boolean op OR")
    (* Integer Ops *)
    | (AST.Int, AST.Op_add, AST.Int) -> [AddInt (Reg r, Reg r, Reg (r+1))]
    | (AST.Int, AST.Op_sub, AST.Int) -> [SubInt (Reg r, Reg r, Reg (r+1))]
    | (AST.Int, AST.Op_mul, AST.Int) -> [MulInt (Reg r, Reg r, Reg (r+1))]
    | (AST.Int, AST.Op_div, AST.Int) -> [DivInt (Reg r, Reg r, Reg (r+1))]
    | (AST.Int, AST.Op_eq, AST.Int) -> [CmpEqInt (Reg r, Reg r, Reg (r+1))]
    | (AST.Int, AST.Op_noteq, AST.Int) -> [CmpNeInt (Reg r, Reg r, Reg (r+1))]
    | (AST.Int, AST.Op_gt, AST.Int) -> [CmpGtInt (Reg r, Reg r, Reg (r+1))]
    | (AST.Int, AST.Op_gteq, AST.Int) -> [CmpGeInt (Reg r, Reg r, Reg (r+1))]
    | (AST.Int, AST.Op_lt, AST.Int) -> [CmpLtInt (Reg r, Reg r, Reg (r+1))]
    | (AST.Int, AST.Op_lteq, AST.Int) -> [CmpLeInt (Reg r, Reg r, Reg (r+1))]
    (* Real Ops *)
    | (AST.Float, AST.Op_add, AST.Float) -> 
                                [AddReal (Reg r, Reg r, Reg (r+1))]
    | (AST.Float, AST.Op_sub, AST.Float) -> 
                                [SubReal (Reg r, Reg r, Reg (r+1))]
    | (AST.Float, AST.Op_mul, AST.Float) -> 
                                [MulReal (Reg r, Reg r, Reg (r+1))]
    | (AST.Float, AST.Op_div, AST.Float) -> 
                                [DivReal (Reg r, Reg r, Reg (r+1))]
    | (AST.Float, AST.Op_eq, AST.Float) -> 
                                [CmpEqReal (Reg r, Reg r, Reg (r+1))]
    | (AST.Float, AST.Op_noteq, AST.Float) -> 
                                [CmpNeReal (Reg r, Reg r, Reg (r+1))]
    | (AST.Float, AST.Op_gt, AST.Float) -> 
                                [CmpGtReal (Reg r, Reg r, Reg (r+1))]
    | (AST.Float, AST.Op_gteq, AST.Float) -> 
                                [CmpGeReal (Reg r, Reg r, Reg (r+1))]
    | (AST.Float, AST.Op_lt, AST.Float) -> 
                                [CmpLtReal (Reg r, Reg r, Reg (r+1))]
    | (AST.Float, AST.Op_lteq, AST.Float) -> 
                                [CmpLeReal (Reg r, Reg r, Reg (r+1))]
    | _ -> 
    raise (Uncaught_Semantic_Error "Binop doesn't take arguments of this type")
  in
  binop_code @ rexpr_code @ lexpr_code
and
gen_expr stbl proc_id load_reg expr =
  match expr with
  | AST.Eint (i, _) -> gen_int_const load_reg i
  | AST.Efloat (n, _) -> gen_real_const load_reg n
  | AST.Ebool (b, _) -> gen_bool_const load_reg b
  | AST.Elval (lval, _) -> gen_lval stbl proc_id load_reg lval
  | AST.Eunop (unop, expr, _) ->
      gen_unop_expr stbl proc_id load_reg unop expr
  | AST.Ebinop (lexpr, binop, rexpr, _) ->
      gen_binop_expr stbl proc_id load_reg binop lexpr rexpr

(*---- Assignment ----*)

(*let gen_assign stbl proc_id reg scope slot expr =
  let slot_num =
    match !slot with
    | Some num -> num
    | None -> raise (Unimplemented "No Slot Assigned")
  in
  let expr_code = gen_expr stbl proc_id reg expr in
  let (Reg r) = reg in
  let next_reg = Reg (r+1) in
  let asgn_code =
    match scope with
    | ST.SDecl
    | ST.SValParam -> [Store (StackSlot slot_num, reg)]
    | ST.SRefParam -> [StoreIndirect (next_reg, reg);
                       Load (next_reg, StackSlot slot_num)]
  in
  asgn_code @ expr_code*)

let gen_lval_assign reg (lscope, lslot) (rscope, rslot) =
  let lslot_num =
    match !lslot with
    | Some slotnum -> slotnum
    | None -> raise (Unimplemented "No lval Slot assigned")
  in
  let rslot_num =
    match !rslot with
    | Some slotnum -> slotnum
    | None -> raise (Unimplemented "No rval Slot assigned")
  in
  let (Reg r) = reg in
  let next_reg = Reg (r+1) in
  match (lscope, rscope) with
  | (Val, Val) -> [Store (StackSlot lslot_num, reg);
                   Load (reg, StackSlot rslot_num)]
  | (Val, Ref) -> [Store (StackSlot lslot_num, reg);
                   LoadIndirect (reg, reg);
                   Load (reg, StackSlot rslot_num)]
  | (Ref, Val) -> [StoreIndirect (next_reg, reg);
                   Load (next_reg, StackSlot lslot_num);
                   (Load (reg, StackSlot rslot_num))]
  | (Ref, Ref) -> [StoreIndirect (next_reg, reg);
                   Load (next_reg, StackSlot lslot_num);
                   LoadIndirect (reg, reg);
                   Load (reg, StackSlot rslot_num)]

let gen_lval_assign_code stbl proc_id lval rval =
  let lpos = AST.get_lval_pos lval in
  let rpos = AST.get_lval_pos rval in
  let lscope = get_pass_type (ST.get_lval_scope stbl proc_id lval lpos) in
  let rscope = get_pass_type (ST.get_lval_scope stbl proc_id rval rpos) in
  let (l_tsym, lslot) = ST.get_lval_sym stbl proc_id lval lpos in
  let (r_tsym, rslot) = ST.get_lval_sym stbl proc_id rval rpos in
  match (l_tsym, r_tsym) with
  | (ST.Tsym _, ST.Tsym _) ->
      gen_lval_assign (Reg 0) (lscope, lslot) (rscope, rslot)

let gen_assign_code stbl proc_id lval rval =
  let lid_assign id =
    let pos = AST.get_lval_pos lval in
    let slot_num = ST.get_id_slot stbl proc_id id pos in
    let scope = ST.get_var_scope stbl proc_id id pos in
    match scope with
    | ST.SDecl
    | ST.SValParam -> [Store (StackSlot slot_num, Reg 0)]
    | ST.SRefParam -> [StoreIndirect (Reg 1, Reg 0);
                       Load (Reg 1, StackSlot slot_num)]
  in
  match rval with
  | AST.Rexpr expr ->
      match expr with
      | AST.Elval (rlval, _) -> gen_lval_assign_code stbl proc_id lval rlval
      | _ -> 
        let expr_code = gen_expr stbl proc_id (Reg 0) expr in
        let asgn_code =
          match lval with
          | AST.LId (id, _) -> lid_assign id
          | AST.LArray _ -> [] (* Array assignment stuff *)
        in
        asgn_code @ expr_code

(*---- IO Write ----*)

let gen_builtin_write t =
  match t with
  | AST.Int -> [CallBuiltin PrintInt]
  | AST.Float -> [CallBuiltin PrintReal]
  | AST.Bool -> [CallBuiltin PrintBool]

let get_write stbl proc_id wrt =
  let print_reg = Reg 0 in
  match wrt with
  | AST.WString (str, _) ->
      [CallBuiltin PrintString; StringConst (print_reg, str)]
  | AST.WExpr expr ->
      let expr_code = gen_expr stbl proc_id print_reg expr in
      let ST.Tsym etype = expr_type stbl proc_id expr in
      let print_code = gen_builtin_write etype in
      print_code @ expr_code



