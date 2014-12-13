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
    structs, etc 
  - any name collision exceptions raised by the Environment module
    will simply cause the generator to fail, reporting the cause of
    the error *)

open Ast;;
open Environment;;

exception SyntaxError of int * int * string;;  

(* generate the C representation of an individual vdecl *)
let c_vdecl_no_semi vdecl =
  let prefix vdecl = match (vdecl.isConst, vdecl.isStruct) with
    (true, true)  -> "const struct "
  | (true, false) -> "const "
  | (false, true) -> "struct "
  | _             -> ""
  in
  (prefix vdecl) ^ vdecl.v_type ^ " " ^ vdecl.v_name
;;							       

(* return updated table and a generated C string of the var-decs *)
let gen_global_vdecls (vdecls, _, _) env =
  let process_vdecl (env, text) vdecl =
    (* will throw NameAlreadyBoundError *)
    (add_var vdecl env, text ^ (c_vdecl_no_semi vdecl) ^ ";\n")
  in
  List.fold_left process_vdecl (env, "") (List.rev vdecls)
;;

(* take in a list of formals and return a c string representation
   e.g. "int a, int b, int c" *)
let rec c_formals = function
    [] -> ""
  | [vdecl] -> (c_vdecl_no_semi vdecl)
  | vdecl :: other_vdecls -> (c_vdecl_no_semi vdecl) ^ ", "
			     ^ (c_formals other_vdecls)
;;				 
  
let c_fdecl fdecl =
  (*TODO Add in function typing *)
  (*fdecl.ftype ^ " " ^ *)
  "F_TYPE " ^ fdecl.fname ^ "(" ^ c_formals fdecl.formals ^ ")" ^ "{}\n"
;;								 
  
let gen_fdecls (_, _, fdecls) env =
  let process_fdecl (env, text) fdecl =
    (* will throw NameAlreadyBoundError *)
    (add_func fdecl env, text ^ (c_fdecl fdecl))
  in
  List.fold_left process_fdecl (env, "") (List.rev fdecls)
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
  let env = Environment.empty() in
  let env, c_vdecls_text = gen_global_vdecls program env in
  let env, c_fdecls_text = gen_fdecls program env in
  print_string c_vdecls_text
;;
