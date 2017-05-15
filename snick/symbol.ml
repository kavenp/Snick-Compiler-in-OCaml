(*---- Snick Symbol Table ----*)

module AST = Snick_ast

(*---- Data Structure ----*)

type ident = AST.ident

type pos = (Lexing.position * Lexing.position)

type type_symbol = AST.snicktype

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
    proc_lable: string option ref;
    proc_pos: pos
  }

(* Symbol table as a hashtable of process symbols for easy retrieval *)
type stbl =
  {
    sprocs: (ident, proc) Hashtbl.t;
  }

type t = stbl

(*---- Exceptions ----*)

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

(*---- Symbol table Functions ----*)

let get_proc_pos stbl id =
  let proc = Hashtbl.find stbl.sprocs id in
  proc.proc_pos

let get_var_sym stbl proc_id var_id =
  let proc = Hashtbl.find stbl.sprocs proc_id in
  Hashtbl.find proc.proc_stbl var_id

let get_lval_sym stbl proc_id lval =
  match lval with
  | AST.LId (id, _) ->
    let (stype, _, slot, _) = get_var_sym stbl proc_id id in
    (stype, slot)
  (* Array symbol stuff here empty for now *)
  | AST.LArray (id, dims, _) -> (AST.Bool, ref None)

let get_id_type stbl proc_id id =
  let proc = Hashtbl.find stbl.sprocs proc_id in
  let (stype, _, _, _) = Hashtbl.find proc.proc_stbl id in
  stype






