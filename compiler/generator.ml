(* generator.ml

  Semantic checking and code generation for Sheets. May later be split
  into two files.
 
  Current Objectives: 
  - generate symbol table for global vdecls 
  - make sure that no variables are reused.
  
  Conventions: 
  - generation functionality will result in a series of
    strings that can be printed to a file or printed to stdout
  - scoped symbol tables will be implemented with a list of
    Map.Make(String) (we may need several maps for variables,
    structs, etc *)

open Parser;;
open Scanner;;
open Ast;;
open Printf;;  

exception SyntaxError of int * int * string;;  

(* generate the C representation of an individual vdecl *)
let c_vdecl vdecl =
  let prefix vdecl = match (vdecl.isConst, vdecl.isStruct) with
    (true, true)  -> "const struct "
  | (true, false) -> "const "
  | (false, true) -> "struct "
  | _             -> ""
  in
  (prefix vdecl) ^ vdecl.v_type ^ " " ^ vdecl.v_name ^ ";\n"
;;							       

(* return updated table and a generated C string of the var-decs *)
let gen_global_vdecls (vdecls, sdefs, fdecls) env =
  let validate_vdecl (env, text) vdecl =
    try
      (add_var vdecl env, text ^ (c_vdecl vdecl))
  in
  let text = "" in       
  List.fold_left validate_vdecl (env, text) (List.rev vdecls)
;;
  
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = try
      Parser.program Scanner.token lexbuf
    with except ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let col = curr.Lexing.pos_cnum in
        let tok = Lexing.lexeme lexbuf in
        raise (SyntaxError (line, col, tok))
  in
  let env = Environment.empty in
  let env, c_vdecls_text = gen_global_vdecls program table in
  print_string c_vdecls_text
;;
