(*===== Snick Symbol Table =====*)

module AST = Snick_ast

(*---- Data Structure ----*)

type ident = AST.ident

type pos = (Lexing.position * Lexing.position)

type type_symbol = 
  | Tsym of AST.snicktype

(* Scope of variables 
 * -Local Declaration
 * -Pass-by-Value parameter
 * -Pass-by-Reference parameter *)
type var_scope =
  | SDecl
  | SValParam
  | SRefParam

(* Variable symbol
 * -type, scope, possible slot and position *)
type var_symbol = (type_symbol * var_scope * int option ref * pos)

(* Process information stored in record 
 * -List of arguments
 * -Process local symbol table
 * -Possible process label
 * -Process position *)
type proc =
  {
    proc_args: AST.ident list;
    proc_stbl: (ident, var_symbol) Hashtbl.t;
    proc_label: string option ref;
    proc_pos: pos
  }

(* Symbol table as a hashtable of process symbols for easy retrieval *)
type stbl =
  {
    sprocs: (ident, proc) Hashtbl.t;
  }

type t = stbl

(*---- Exceptions ----*)

(* Symbol table definition error *)
exception Definition_error of string * AST.pos
(* Non-unique process name in symbol table *)
exception Duplicate_proc of string * AST.pos
(* Non-unique arg name in symbol table *)
exception Duplicate_arg of string * AST.pos
(* Non-unique variable name in symbol table *)
exception Duplicate_decl of string * AST.pos

exception Undefined_variable of string * AST.pos
exception Undefined_process of string * AST.pos
(* No slot to store value *)
exception No_allocated_Slot


(*---- Helper functions ----*)
let find_proc stbl proc_id pos =
  let proc =
      try
        Hashtbl.find stbl.sprocs proc_id
      with
      | Not_found -> raise (Undefined_process (proc_id, pos))
  in
  proc

(*---- Symbol table Functions ----*)

let get_proc_pos stbl id pos =
  let proc = find_proc stbl id pos in
  proc.proc_pos

let get_var_sym stbl proc_id var_id pos =
  let proc = find_proc stbl proc_id pos in
  Hashtbl.find proc.proc_stbl var_id

let get_lval_sym stbl proc_id lval pos =
  match lval with
  | AST.LId (id, _) ->
    let (stype, _, slot, _) = get_var_sym stbl proc_id id pos in
    (stype, slot)
  (* Array symbol stuff here empty for now *)
  | AST.LArray (id, dims, _) -> (Tsym AST.Bool, ref None)

let get_id_type stbl proc_id id pos =
  let proc = find_proc stbl proc_id pos in
  let (stype, _, _, _) = Hashtbl.find proc.proc_stbl id in
  stype

let get_lval_type stbl proc_id lval pos =
  let (stype, _) = get_lval_sym stbl proc_id lval pos in
  stype

let set_id_slot stbl proc_id id slot pos =
  let proc = find_proc stbl proc_id pos in
  let (_, _, slot_num, _) = Hashtbl.find proc.proc_stbl id in
  slot_num := Some slot;
  slot+1

let get_id_slot stbl proc_id id pos =
  let proc = find_proc stbl proc_id pos in
  let (_, _, slot, _) = Hashtbl.find proc.proc_stbl id in
  match !slot with
  | None -> raise No_allocated_Slot
  | Some n -> n

let set_proc_label stbl proc_id label pos =
  let proc = find_proc stbl proc_id pos in
  proc.proc_label := Some label

let get_proc_label stbl proc_id pos =
  let proc = find_proc stbl proc_id pos in
  match !(proc.proc_label) with
  | None -> raise (Undefined_process (proc_id, pos))
  | Some label -> label

let get_args stbl proc_id pos =
  let proc = find_proc stbl proc_id pos in
  proc.proc_args

(* Get a variable's scope given the containing process and var id *)
let get_var_scope stbl proc_id id pos =
  let proc = find_proc stbl proc_id pos in
  let proc_stbl = proc.proc_stbl in
  let (_, scope, _, _) = Hashtbl.find proc_stbl id in
  scope

let get_lval_scope stbl proc_id lval pos =
  match lval with
  | AST.LId (id, _) -> get_var_scope stbl proc_id id pos
  (* Array Lval here as well, placeholder *)
  | AST.LArray (id, dims, _) -> SDecl

(*---- Constructors ----*)
(* Function to convert AST type to type symbol *)
let make_type_sym snicktype =
  Tsym snicktype

let make_var_sym tsym scope pos =
  (tsym , scope, ref None, pos)

let make_decl_sym decl_type pos =
  let tsym = make_type_sym decl_type in
  make_var_sym tsym SDecl pos

let add_decl_sym proc_stbl decl =
  match decl with
  | AST.RegDecl (id, decl_type, pos) ->
    let decl_sym = make_decl_sym decl_type pos in
    if Hashtbl.mem proc_stbl id then
      raise (Duplicate_decl (id, pos))
    else
      Hashtbl.add proc_stbl id decl_sym
  (* Array declaration placeholder *)
  | AST.ArrayDecl (id, decl_type, intervals, pos) -> ()

let make_arg_sym arg_pass arg_type pos =
  let tsym = make_type_sym arg_type in
  let arg_scope =
    match arg_pass with
    | AST.Val -> SValParam
    | AST.Ref -> SRefParam
  in
  make_var_sym tsym arg_scope pos

let add_arg_sym proc_stbl arg =
  let (arg_pass, arg_type, id, pos) = arg in
  let arg_sym = make_arg_sym arg_pass arg_type pos in
  if Hashtbl.mem proc_stbl id then
    raise (Duplicate_arg (id, pos))
  else
    Hashtbl.add proc_stbl id arg_sym

let add_proc stbl proc =
  let (proc_id, args, (decls, _), pos) = proc in
  if Hashtbl.mem stbl proc_id then
    raise (Duplicate_proc (proc_id, pos))
  else
    let get_arg_ids (_, _, id, _) ids = id :: ids in
    let proc_args = List.fold_right get_arg_ids args [] in
    let proc_stbl = Hashtbl.create 10 in
    let proc_label = ref None in
    let add_arg arg = add_arg_sym proc_stbl arg in
    let add_decl decl = add_decl_sym proc_stbl decl in
    List.iter add_arg args;
    List.iter add_decl decls;
    let proc_sym = {
                      proc_args = proc_args;
                      proc_stbl = proc_stbl;
                      proc_label = proc_label;
                      proc_pos = pos
                   }
    in
    Hashtbl.add stbl proc_id proc_sym

let add_procs stbl procs =
  List.iter (add_proc stbl) procs

let build_stbl ast =
  let stbl = Hashtbl.create 10 in
  add_procs stbl ast.AST.procs;
  { sprocs = stbl }

let build_stbl_checks ast =
  try
    build_stbl ast
  with
  | Duplicate_proc (id, pos) ->
    raise (Definition_error ("Type " ^id^ " is already defined.", pos))
  | Duplicate_arg (id, pos) ->
    raise (Definition_error ("Argument " ^id^ " is already passed.", pos))
  | Duplicate_decl (id, pos) ->
    raise (Definition_error ("Variable " ^id^ " is already declared.", pos))
  | Undefined_variable (id, pos) ->
    raise (Definition_error ("Variable " ^id^ " is undefined.", pos))
  | Undefined_process (id, pos) ->
    raise (Definition_error ("Process " ^id^ " is undefined.", pos))