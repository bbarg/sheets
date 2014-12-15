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
open Printf;;

exception SyntaxError of int * int * string;;
exception NotImplementedError of string;;
exception UndefinedTypeError;;

let generate_exp exp env = 
    match exp with
      Literal_int(i) -> Environment.append env [Text(string_of_int(i) ) ] 
      | Literal_float(f) -> Environment.append env [Text(string_of_float(f) )]
      | Literal_int_a(int_a) -> raise (NotImplementedError("int array literal"))
      | Literal_float_a(float_a) -> raise (NotImplementedError("float array literal"))
      | Id(s) -> Environment.append env [Text(s) ] 
      | Binop(e1, op, e2) -> raise (NotImplementedError("binop"))
      | _-> raise (NotImplementedError("unsupported expression"))
;;
       
let rec generate_type datatype env = 
   match datatype with 
	| Int -> Environment.append env [Text("int")] 
	| Float -> Environment.append env [Text("float")] 
	| Array(t) -> Environment.append env [ 
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
(*     Vdecl(vdecl) -> Environment.append env [ Generator(process_vdecl vdecl) ] *)
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
(*   Environment.append env [ *)
(* 			Generator(generate_type v_datatype); *)
(* 			Text(" " ^ vdecl.v_name ^ ";"); *)
(* 			Generator(add_var vdecl.v_name v_datatype) *)
(* 		      ] *)
(* ;; *)
(* ------------------------------------------------------------------ *)

(* ------------------------------------------------------------------ *)
(* Global Variable Declarations                                       *)
(* ------------------------------------------------------------------ *)

let rec generate_global_vdecl_list vdecls env =
  let generate_global_vdecl vdecl env =
    let v_datatype = Generator_utilities.str_to_type vdecl.v_type in
    Environment.append env [Env((add_var vdecl.v_name v_datatype));
			    Generator(generate_type v_datatype);
			    Text(" " ^ vdecl.v_name ^ ";\n")]
  in
  match vdecls with
    [] -> "", env
  | [vdecl] -> generate_global_vdecl vdecl env
  | vdecl :: other_vdecls ->
     Environment.append env [Generator(generate_global_vdecl vdecl);
			     Generator(generate_global_vdecl_list other_vdecls)]
;;

(* ------------------------------------------------------------------ *)
(* CPU functions                                                      *)
(* ------------------------------------------------------------------ *)
let rec generate_formals_vdecl_list vdecl_list env =  
    let generate_formals_vdecl vdecl env =
       let v_datatype = Generator_utilities.str_to_type vdecl.v_type in
    Environment.append env [Env((add_var vdecl.v_name v_datatype));
			    Generator(generate_type v_datatype);
			    Text(" " ^ vdecl.v_name ^ ", ")]
  in
  match vdecl_list with
    [] -> "", env
  | [vdecl] -> Environment.append env [Env((add_var vdecl.v_name (Generator_utilities.vdecl_type vdecl)));
			    Text(vdecl.v_type ^ " " ^ vdecl.v_name)]

  | vdecl :: other_vdecls ->
     Environment.append env [Generator(generate_formals_vdecl vdecl);
			     Generator(generate_formals_vdecl_list other_vdecls)]
;;


let rec generate_cpu_funcs fdecls env =
  let generate_cpu_func fdecl env =
    match fdecl.isGfunc with
      false -> 
      Environment.append env [Env(add_func fdecl.fname (Generator_utilities.fdecl_to_func_info fdecl) );
			      Text(fdecl.r_type ^ " " ^ fdecl.fname ^ "(");
			      NewScope(generate_formals_vdecl_list fdecl.formals );
			      (* TODO Here is where we parse the body of the function??
			       *)
			      Text("){\n");
			      (* TODO: here is where we call stmt proc *) 
			      Text("}\n"); 
			     ]
                         
    | true -> "", add_gfunc (Generator_utilities.fdecl_to_func_info fdecl) env
  in
  match fdecls with
    [] -> "", env
  | [fdecl] -> generate_cpu_func fdecl env
  | fdecl :: other_fdecls ->
     Environment.append env [Generator(generate_cpu_func fdecl);
			     Generator(generate_cpu_funcs other_fdecls);]
;;

(* ------------------------------------------------------------------ *)
(* OpenCL Kernels                                                     *)
(* ------------------------------------------------------------------ *)
(* Each gfunc has a requires a set of variables to access its
   associated OpenCL kernel representation. For a gfunc called
   "mygfunc", these variables are:
   - `mygfunc_kernel_string'  : a string of the opencl kernel code
   - `mygfunc_kernel_name'    : a string of the function name;
                                circuitously, this will be "mygfunc"
   - `mygfunc_compiled_kernel : the compiled cl_kernel object

   We have to declare all of these variable globally (and at the top
   of our generated c program) so they will be accessible from any
   cpu function. 

   ASSUMPTIONS:
   - the incoming func_info list will not include any invalid names
     (i.e. there won't be a gfunc called main) *)
  
let gfunc_to_cl_kernel_string gf_info env =
  (* we have to reject all references to variables that aren't
       immediately in scope *)
  (* we're going to have to escape double-quotes when we write
       these string literals *)
  (* TODO; returning test string for testing *)
  
  
  "TODO: cl_kernel string goes here", env

let gfunc_to_cl_kernel gf_info env =
  Environment.append env [Text(sprintf "const char *%s_kernel_string = \"" gf_info.id);
			  (* we aren't ever changing the environment
			      above the gfunc's scope, but we need to
			      generate a new scope to parse the gfunc's contents *)
			  NewScope(gfunc_to_cl_kernel_string gf_info);
			  Text("\";\n");
			  Text(sprintf "const char *%s_kernel_name = \"%s\";\n"
				       gf_info.id gf_info.id);
			  Text(sprintf "cl_kernel %s_compiled_kernel;\n" gf_info.id)]

let rec gfunc_list_to_cl_kernels gf_info_list env =
  match gf_info_list with
    [] -> "", env
  | gf_info :: other_gf_infos ->
     Environment.append env [Generator(gfunc_to_cl_kernel gf_info);
			     Generator(gfunc_list_to_cl_kernels other_gf_infos)]

let generate_cl_kernels env =
  let cl_globals = "cl_context __sheets_context;\n" 
		   ^ "cl_command_queue __sheets_queue;\n"
		   ^ "cl_int __cl_err;\n"
  in
  Environment.append env [Text(cl_globals);
			  Generator(gfunc_list_to_cl_kernels env.gfunc_list)]

(* ------------------------------------------------------------------ *)
(* Main: opencl context creation and frees                            *)
(* ------------------------------------------------------------------ *)

(* ASSUMPTIONS: 
   - the incoming func_info list will not include any invalid names
     (i.e. there won't be a gfunc called main) *)


let rec generate_compile_kernels gf_info_list env =
  let generate_compile_kernel gf_info =
    sprintf "%s_compiled_kernel = kernel_from_string(__sheets_context, %s_kernel_string, %s_kernel_name, SHEETS_KERNEL_COMPILE_OPTS);\n" gf_info.id gf_info.id gf_info.id
  in
  match gf_info_list with
    [] -> "", env
  | gf_info :: other_gf_infos ->
     Environment.append env [Text(generate_compile_kernel gf_info);
			     Generator(generate_compile_kernels other_gf_infos)]
			
let rec generate_release_kernels gf_info_list env =
  let generate_release_kernel gf_info =
    sprintf "CL_CALL_GUARDED(clReleaseKernel, (%s_compiled_kernel));\n" gf_info.id
  in
  match gf_info_list with
  [] -> "", env
  | gf_info :: other_gf_infos ->
     Environment.append env [Text(generate_release_kernel gf_info);
			     Generator(generate_release_kernels other_gf_infos)]

let generate_main env =
  Environment.append env [Text("int main()\n");
			  Text("{\n");
			  Text("create_context_on(SHEETS_PLAT_NAME, SHEETS_DEV_NAME, 0, &__sheets_context, &__sheets_queue, 0);\n");
			  Generator(generate_compile_kernels env.gfunc_list);
			  Text("snuggle();\n");
			  Generator(generate_release_kernels env.gfunc_list);
			  Text("CALL_CL_GUARDED(clReleaseCommandQueue, (__sheets_queue));\n");
			  Text("CALL_CL_GUARDED(clReleaseContext, (__sheets_context));\n");
			  Text("}\n")]
		     
(* ------------------------------------------------------------------ *)
(* Parse and print                                                    *)
(* ------------------------------------------------------------------ *)
			 
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
  (* TODO find cleaner solution for getting vdecls/fdecls in correct order *)
  let global_vdecls, env = generate_global_vdecl_list (List.rev vdecls) env in
  let cpu_funcs, env = generate_cpu_funcs (List.rev fdecls) env in
  let cl_kernels, env = generate_cl_kernels env in
  let main, env = generate_main env in
  print_string ("#include <stdio.h>\n"
		^ "#include \"aws-g2.2xlarge.h\"\n"
		^ "#include \"cl-helper.h\"\n"
		^ "#include <CL/cl.h>\n\n\n\n");
  print_string cl_kernels;
  print_string global_vdecls;
  print_string cpu_funcs;
  print_string main
;; 

