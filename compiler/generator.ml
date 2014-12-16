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


(* TODO: Strategy for syntax checking 
 * generate_expr will be a set of case matchings that will
 * call gen_checked_<EXPRESSION_NAME> that can meaningfully be 
 * checked 
 * gen_checked_<EXPRESSION_NAME> will take a
        * EXAMPLE: gen_checked_id 
        * function <CHECKER> that returns a boolean 
        * an expression 
        * env 
 * <CHECKER> can be in Environment as it is for variable ids 
 * <CHECKER> takes 
        * an expression 
        * and env 
 * or in Generator_utilities 
 * Maybe there can be one universal <CHECKER> in generate_utilities 
 * And that thing returns true/false 
 * and from there generate_checked_<EXPRESSION_NAME> either 
 * returns a string, env tuple 
 * or makes recursive calls to generate_expression  
 *)
let generate_checked_id check_id id env = 
    if (check_id id env)  then id, env 
    else raise (VariableNotFound id)

let generate_exp exp env = 
    match exp with
      Literal_int(i) -> string_of_int(i), env  
      | Literal_float(f) -> string_of_float(f), env 
      | Literal_int_a(int_a) -> raise (NotImplementedError("int array literal"))
      | Literal_float_a(float_a) -> raise (NotImplementedError("float array literal"))
      | Id(s) -> Environment.append env [Generator(generate_checked_id is_var_in_scope s )]  
      | Binop(e1, op, e2) -> raise (NotImplementedError("binop"))
      | _-> raise (NotImplementedError("unsupported expression"))
;;
       
let rec generate_type datatype env = 
   match datatype with 
	| Int -> "int", env 
	| Float -> "float", env 
	| Array(t) -> Environment.append env [ 
		Generator(generate_type t); 
		Text("[]")
	]
	(* | _ -> raise UndefinedTypeError (\* TODO this should never happen *\) *)

(* ------------------------------------------------------------------ *)		  
(* TODO combine-ify all of these functions *)
					  
let rec process_stmt_list stmt_list env = 
   match stmt_list with 
     []     -> "\n", env (* TODO this is a sanity check *) 
   | stmt :: other_stmts -> process_stmt stmt env; 
                            process_stmt_list other_stmts env; 
 and process_stmt stmt env = 
   match stmt with 
     Vdecl(vdecl) -> Environment.append env [ Generator(process_vdecl vdecl) ] 
   | Block(stmt_list) -> Environment.append env [ Generator(process_stmt_list stmt_list) ] (* TODO check if we need braces/NewScope *) 
   | Expr(expr) -> Environment.append env [ Generator(generate_exp expr ) ]  
   | Assign(name, expr) -> raise (NotImplementedError("assign")) 
   | Return(expr) -> raise (NotImplementedError("expr")) 
   | Init(vdecl, expr) -> raise (NotImplementedError("init and assign")) 
   | If(expr, bool_stmt, body) -> raise (NotImplementedError("if/else")) 
   | While(expr, stmt) -> raise (NotImplementedError("while")) 
   | ForIn(obj, container, stmt) -> raise (NotImplementedError("for in")) 
   | Continue -> raise (NotImplementedError("continue")) 
   | Break ->    raise (NotImplementedError("break"))
   | _ -> raise (NotImplementedError("Undefined type of expression")) 
 and process_vdecl vdecl env = 
   let v_datatype = Generator_utilities.str_to_type vdecl.v_type in 
   Environment.append env [
                        Env(add_var vdecl.v_name v_datatype);
 	                Text(vdecl.v_type ^ " " ^ vdecl.v_name ^ ";\n "); 
 		          ] 
 ;; 
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
                                   
     | true  -> "", env (* TODO in the future handle this *) 
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

let generate_cl_kernels env = "", env
;;				    

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
  print_string ("#include <stdio.h>\n"
		^ "#include \"aws-g2.2xlarge.h\"\n"
		^ "#include \"cl-helper.h\"\n"
		^ "#include <CL/cl.h>\n\n\n\n");
  print_string cl_kernels;
  print_string global_vdecls;
  print_string cpu_funcs;
;; 

