module CG = Codegen

let write = Printf.sprintf

let write_push_stack frame_size =
  write "push_stack_frame %d" frame_size

let write_pop_stack frame_size =
  write "pop_stack_frame %d" frame_size

let write_load (CG.Reg r) (CG.StackSlot s) =
  write "load r%d, %d" r s

let write_store (CG.StackSlot s) (CG.Reg r) =
  write "store %d, r%d" s r

let write_load_addr (CG.Reg r) (CG.StackSlot s) =
  write "load_address r%d, %d" r s

let write_load_ind (CG.Reg r1) (CG.Reg r2) =
  write "load_indirect r%d, r%d" r1 r2

let write_store_ind (CG.Reg r1) (CG.Reg r2) =
  write "store_indirect r%d, r%d" r1 r2

let write_int_const (CG.Reg r) i =
  write "int_const r%d, %d" r i

let write_real_const (CG.Reg r) n =
  write "real_const r%d, %f" r n

let write_str_const (CG.Reg r) str =
  write "string_const r%d, \"%s\"" r str

let write_add_int (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "add_int r%d, r%d, r%d" r1 r2 r3

let write_add_real (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "add_real r%d, r%d, r%d" r1 r2 r3

let write_add_offset (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "add_offset r%d, r%d, r%d" r1 r2 r3

let write_sub_int (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "sub_int r%d, r%d, r%d" r1 r2 r3

let write_sub_real (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "sub_real r%d, r%d, r%d" r1 r2 r3

let write_sub_offset (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "sub_offset r%d, r%d, r%d" r1 r2 r3

let write_mul_int (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "mul_int r%d, r%d, r%d" r1 r2 r3

let write_mul_real (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "mul_real r%d, r%d, r%d" r1 r2 r3

let write_div_int (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "div_int r%d, r%d, r%d" r1 r2 r3

let write_div_real (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "div_real r%d, r%d, r%d" r1 r2 r3

let write_cmp_eq_int (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_eq_int r%d, r%d, r%d" r1 r2 r3

let write_cmp_neq_int (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_ne_int r%d, r%d, r%d" r1 r2 r3

let write_cmp_gt_int (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_gt_int r%d, r%d, r%d" r1 r2 r3

let write_cmp_geq_int (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_ge_int r%d, r%d, r%d" r1 r2 r3

let write_cmp_lt_int (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_lt_int r%d, r%d, r%d" r1 r2 r3

let write_cmp_leq_int (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_le_int r%d, r%d, r%d" r1 r2 r3

let write_cmp_eq_real (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_eq_real r%d, r%d, r%d" r1 r2 r3

let write_cmp_neq_real (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_ne_real r%d, r%d, r%d" r1 r2 r3

let write_cmp_gt_real (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_gt_real r%d, r%d, r%d" r1 r2 r3

let write_cmp_geq_real (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_ge_real r%d, r%d, r%d" r1 r2 r3

let write_cmp_lt_real (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_lt_real r%d, r%d, r%d" r1 r2 r3

let write_cmp_leq_real (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "cmp_le_real r%d, r%d, r%d" r1 r2 r3

let write_and (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "and r%d, r%d, r%d" r1 r2 r3

let write_or (CG.Reg r1) (CG.Reg r2) (CG.Reg r3) =
  write "or r%d, r%d, r%d" r1 r2 r3

let write_not (CG.Reg r1) (CG.Reg r2) =
  write "not r%d, r%d" r1 r2

let write_int_to_real (CG.Reg r1) (CG.Reg r2) =
  write "int_to_real r%d, r%d" r1 r2

let write_move (CG.Reg r1) (CG.Reg r2) =
  write "move r%d, r%d" r1 r2

let write_branch_true (CG.Reg r) (CG.Label label) =
  write "branch_on_true r%d, %s" r label

let write_branch_false (CG.Reg r) (CG.Label label) =
  write "branch_on_false r%d, %s" r label

let write_branch_uncond (CG.Label label) =
  write "branch_uncond %s" label

let write_call (CG.Label label) =
  write "call %s" label

let write_call_builtin builtin =
  let builtin_str =
    match builtin with
    | CG.ReadInt     -> "read_int"
    | CG.ReadReal    -> "read_real"
    | CG.ReadBool    -> "read_bool"
    | CG.PrintInt    -> "print_int"
    | CG.PrintReal   -> "print_real"
    | CG.PrintBool   -> "print_bool"
    | CG.PrintString -> "print_string"
  in
  write "call_builtin %s" builtin_str

let write_return =
  write "return"

let write_halt =
  write "halt"

let write_block_label (CG.Label label) =
  write "%s:" label

let write_debug_reg (CG.Reg r) =
  write "debug_reg r%d" r

let write_debug_slot (CG.StackSlot s) =
  write "debug_slot %d" s

let write_debug_stack =
  write "debug_stack"

let write_instr instr =
  match instr with
  (* Stack Manipulation *)
  | CG.PushStackFrame  frame_size -> write_push_stack frame_size
  | CG.PopStackFrame   frame_size -> write_pop_stack frame_size
  (* Register Store/Load *)
  | CG.Load          (reg0, slot) -> write_load      reg0 slot
  | CG.Store         (slot, reg0) -> write_store     slot reg0
  | CG.LoadAddress   (reg0, slot) -> write_load_addr reg0 slot
  | CG.LoadIndirect  (reg1, reg2) -> write_load_ind  reg1 reg2
  | CG.StoreIndirect (reg1, reg2) -> write_store_ind reg1 reg2
  (* Register Immediate Operation *)
  | CG.IntConst        (reg, i) -> write_int_const reg i
  | CG.RealConst       (reg, n) -> write_real_const reg n
  | CG.StringConst     (reg, str) -> write_str_const reg str
  (* Integer Arithmetic Operations *)
  | CG.AddInt        (r1, r2, r3) -> write_add_int r1 r2 r3
  | CG.SubInt        (r1, r2, r3) -> write_sub_int r1 r2 r3
  | CG.MulInt        (r1, r2, r3) -> write_mul_int r1 r2 r3
  | CG.DivInt        (r1, r2, r3) -> write_div_int r1 r2 r3
  (* Real Arithmetic Operations *)
  | CG.AddReal       (r1, r2, r3) -> write_add_real r1 r2 r3
  | CG.SubReal       (r1, r2, r3) -> write_sub_real r1 r2 r3
  | CG.MulReal       (r1, r2, r3) -> write_mul_real r1 r2 r3
  | CG.DivReal       (r1, r2, r3) -> write_div_real r1 r2 r3
  (* Address Offset Operations *)
  | CG.AddOffset     (r1, r2, r3) -> write_add_offset r1 r2 r3
  | CG.SubOffset     (r1, r2, r3) -> write_sub_offset r1 r2 r3
  (* Integer Comparison Operations *)
  | CG.CmpEqInt      (r1, r2, r3) -> write_cmp_eq_int  r1 r2 r3
  | CG.CmpNeInt      (r1, r2, r3) -> write_cmp_neq_int r1 r2 r3
  | CG.CmpLtInt      (r1, r2, r3) -> write_cmp_lt_int  r1 r2 r3
  | CG.CmpLeInt      (r1, r2, r3) -> write_cmp_leq_int r1 r2 r3
  | CG.CmpGtInt      (r1, r2, r3) -> write_cmp_gt_int  r1 r2 r3
  | CG.CmpGeInt      (r1, r2, r3) -> write_cmp_geq_int r1 r2 r3
  (* Real Comparison Operations *)
  | CG.CmpEqReal     (r1, r2, r3) -> write_cmp_eq_real r1 r2 r3
  | CG.CmpNeReal     (r1, r2, r3) -> write_cmp_neq_real r1 r2 r3
  | CG.CmpLtReal     (r1, r2, r3) -> write_cmp_lt_real r1 r2 r3
  | CG.CmpLeReal     (r1, r2, r3) -> write_cmp_leq_real r1 r2 r3
  | CG.CmpGtReal     (r1, r2, r3) -> write_cmp_gt_real r1 r2 r3
  | CG.CmpGeReal     (r1, r2, r3) -> write_cmp_geq_real r1 r2 r3
  (* Boolean Arithmetic Operations *)
  | CG.And           (r1, r2, r3) -> write_and r1 r2 r3
  | CG.Or            (r1, r2, r3) -> write_or  r1 r2 r3
  | CG.Not               (r1, r2) -> write_not r1 r2
  (* Int to Float Conversion *)
  | CG.IntToReal         (r1, r2) -> write_int_to_real r1 r2
  (* Move *)
  | CG.Move              (r1, r2) -> write_move r1 r2
  (* Branch Instructions *)
  | CG.BranchOnTrue  (reg, label) -> write_branch_true   reg label
  | CG.BranchOnFalse (reg, label) -> write_branch_false  reg label
  | CG.BranchUncond         label -> write_branch_uncond label
  (* Calls *)
  | CG.Call                 label -> write_call         label
  | CG.CallBuiltin        builtin -> write_call_builtin builtin
  | CG.Return                     -> write_return
  (* Emulator Halt *)
  | CG.Halt                       -> write_halt
  | CG.BlockLabel           label -> write_block_label label
  (* Debug Instructions *)
  | CG.DebugReg               reg -> write_debug_reg   reg
  | CG.DebugSlot       stack_slot -> write_debug_slot  stack_slot
  | CG.DebugStack                 -> write_debug_stack

let generate_brill_code code =
  String.concat "\n" (List.map write_instr code)