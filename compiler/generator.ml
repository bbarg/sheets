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
exception NotImplementedError of string;;
exception UndefinedTypeError;;

let generate_exp exp env = 
    match exp with
      Literal_int(i) -> Environment.combine env [Text(string_of_int(i) ) ] 
      | Literal_float(f) -> Environment.combine env [Text(string_of_float(f) )]
      | Literal_int_a(int_a) -> raise (NotImplementedError("int array literal"))
      | Literal_float_a(float_a) -> raise (NotImplementedError("float array literal"))
      | Id(s) -> Environment.combine env [Text(s) ] 
      | Binop(e1, op, e2) -> raise (NotImplementedError("binop"))
      | _-> raise (NotImplementedError("unsupported expression"))
;;
       
let rec generate_type datatype env = 
   match datatype with 
	| Int -> Environment.combine env [Text("int")] 
	| Float -> Environment.combine env [Text("float")] 
	| Array(t) -> Environment.combine env [ 
		Generator(generate_type t); 
		Text("[]")
	]
	(* | _ -> raise UndefinedTypeError (\* TODO this should never happen *\) *)

(* ------------------------------------------------------------------ *)		  
(* TODO combine-ify all of these functions *)
					  
(* let rec process_stmt_list stmt_list env = *)
(*   match stmt_list with *)
(*     []     -> "", env *)
(*   | [stmt] -> process_stmt (env, text) stmt *)
(*   | stmt :: other_stmts -> process_stmt_list *)
(* 			     (process_stmt (env, text) stmt) *)
(* 			     other_stmts *)
(* and process_stmt stmt env = *)
(*   match stmt with *)
(*     Vdecl(vdecl) -> Environment.combine env [ Generator(process_vdecl vdecl) ] *)
(*   | Block(stmt_list) -> process_stmt_list  stmt_list *)
(*   | Expr(expr) -> raise (NotImplementedError("expr")) *)
(*   | Assign(name, expr) -> raise (NotImplementedError("assign")) *)
(*   | Return(expr) -> raise (NotImplementedError("expr")) *)
(*   | Init(vdecl, expr) -> raise (NotImplementedError("init and assign")) *)
(*   | If(expr, bool_stmt, body) -> raise (NotImplementedError("if/else")) *)
(*   | While(expr, stmt) -> raise (NotImplementedError("while")) *)
(*   | ForIn(obj, container, stmt) -> raise (NotImplementedError("for in")) *)
(*   | Continue -> (env, text ^ "continue;\n") *)
(*   | Break -> (env, text ^ "break:\n") *)
(*   | _ -> raise (NotImplementedError("Undefined type of expression")) *)
(* and process_vdecl vdecl env = *)
(*   let v_datatype = Generator_utilities.str_to_type vdecl.vtype in *)
(*   Environment.combine env [ *)
(* 			Generator(generate_type v_datatype); *)
(* 			Text(" " ^ vdecl.v_name ^ ";"); *)
(* 			Generator(add_var vdecl.v_name v_datatype) *)
(* 		      ] *)
(* ;; *)
(* ------------------------------------------------------------------ *)

(* ------------------------------------------------------------------ *)
(* NEW GENERATOR CODE                                                 *)
(* ------------------------------------------------------------------ *)

let rec generate_global_vdecl_list vdecls env =
  let generate_global_vdecl vdecl env =
    let v_datatype = Generator_utilities.str_to_type vdecl.vtype in
    Environment.combine env [
			  Generator(generate_type v_datatype);
			  Text(" " ^ vdecl.v_name ^ ";");
			  Generator(add_var vdecl.v_name v_datatype)
			]
  in
  match vdecls with
    [] ->
    Environment.combine env [
			  Text("")
			]
  | vdecl ->
     Environment.combine env [
			   Generator(generate_global_vdecl vdecl)
			 ]
  | vdecl :: other_vdecls ->
     Environment.combine env [
			   Generator(generate_global_vdecl vdecl);
			   Generator(generate_global_vdecl_list other_vdecls)
			 ]
  
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let vdecls, _, fdecls = try
      Parser.program Scanner.token lexbuf
    with except ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let col = curr.Lexing.pos_cnum in
        let tok = Lexing.lexeme lexbuf in
        raise (SyntaxError (line, col, tok))
  in
  let env = Environment.create in
  let global_vdecls, env = generate_global_vdecl_list vdecls env in
  let cpu_funcs, env = generate_cpu_funcs fdecls env in
  let cl_kernels = generate_cl_kernels env in
  print_string "#include <stdio.h>\n" ^ "#include \"aws-g2.2xlarge.h\"\n" ^
    "#include \"cl-helper.h\"" ^ "include <CL/cl.h"
  print_string cl_kernels;
  print_string global_vdecls;
  print_string cpu_funcs;
;; 

