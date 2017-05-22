(* ----------------------------------------------------- | 
 * Main Module for Snick language Compiler               |
 * ----------------------------------------------------- |
 * This module takes a program file or stdin             |
 * and parses flags to decide whether to pretty print    |
 * or compile the provided Snick program                 |
 * ----------------------------------------------------- | *)

 (* Team : PSZ *)
 (* Authors : Kaven Peng --usrname: kavenp --ID: 696573 *)
 (*           Dingli Zhao --usrname: dingliz --ID: 809593 *)
 (*           Jiahao Shi --usrname: jiahaos1 --ID: 804940 *)

module P = Snick_parse

(* Argument parsing code *)
let infile_name = ref None
let outfile_name = ref None

type compiler_mode = PrettyPrint | Compile
let mode = ref Compile

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  [("-p",
     Arg.Unit(fun () -> mode := PrettyPrint),
     " Run the compiler in pretty-printer mode");
   ("-o",
     Arg.String(fun str -> outfile_name := Some str),
     " Specify output file for compilation")
  ]

let main () =
  (* Parse the command-line arguments *)
  Arg.parse speclist
      (begin fun fname -> infile_name := Some fname end)
      "snick [-p] [snick source]" ;
  (* Open the input file *)
  let infile = 
    match !infile_name with
    | None -> stdin
    | Some fname -> open_in fname in
  (* Initialize lexing buffer *)
  let lexbuf = Lexing.from_channel infile in
  (* Call the parser *)
try
  let prog = Snick_parse.program Snick_lex.token lexbuf in
  let stbl = Symbol.build_stbl_checks prog in
  let outfile =
    match !outfile_name with
    | Some filename -> open_out filename
    | None          -> stdout
  in
  match !mode with
  | PrettyPrint ->
    Snick_pprint.print_program Format.std_formatter prog 
  | Compile -> 
      let ir_code = Codegen.gen_code stbl prog in
      let brill_code = Snick_brill.generate_brill_code ir_code in
      try
        Printf.fprintf outfile "%s" brill_code;
        close_out outfile
      with e ->
        close_out_noerr outfile;
        raise e
with
  (* Error reporting with line numbers and columns *)
  | Snick_lex.Lex_error msg -> (* Lexing error *)
      let (ln, col) = Snick_lex.get_lex_pos lexbuf in
      Printf.fprintf stderr
        "Lexer error: %s at line %i, column %i\n" msg ln col;
      exit 1
  | Parsing.Parse_error -> (* Parsing error *)
      let (ln, col) = Snick_lex.get_lex_pos lexbuf in
      Printf.fprintf stderr
        "Parse error at line %i, column %i\n" ln col;
      exit 1
  | Symbol.Definition_error (msg, pos) ->
      let ((st_ln, st_col), (end_ln, end_col)) =
        Snick_ast.get_pos_info pos
      in
      Printf.fprintf stderr
        "%s: from line %i, column %i to line %i, column %i\n"
        msg st_ln st_col end_ln end_col;
      exit 1
  | Analyze.Semantic_error (msg, pos) ->
      let ((st_ln, st_col), (end_ln, end_col)) =
        Snick_ast.get_pos_info pos
      in
      Printf.fprintf stderr
        "%s: from line %i, column %i to line %i, column %i\n"
        msg st_ln st_col end_ln end_col;
      exit 1
  | Analyze.Missing_Main ->
      Printf.fprintf stderr "No main procedure defined";
      exit 1
  
let _ = main ()
