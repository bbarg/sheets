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
open String;;

exception SyntaxError of int * int * string;;
exception NotImplementedError of string;;
exception UndefinedTypeError;;
exception BadExpressionError of string;;
  
let print_vdecls vdecls = 
  let f v =
    printf "DEBUG printing vdecl: v_type: %s, v_name: %s, isConst: %B, isStruct %B\n"
	   v.v_type v.v_name v.isConst v.isStruct
  in List.iter f vdecls
;;

let print_sdefs sdefs =
  let f s =
    printf "DEBUG printing sdefs: s_name=%s: s_elements=<"
	   s.s_name;
    print_vdecls s.s_elements;
    printf ">\n";
  in List.iter f sdefs
;;


let print_ops = function
      Plus -> printf "+"
    | Minus -> printf "-"
    | Times -> printf "*"
    | Divide -> printf "/"
    | Mod ->printf "%c" '%'
    | Neg ->printf "-"
    | Equal ->printf "=="
    | Neq -> printf"!="
    | Less ->printf "<"
    | Leq ->printf "<="
    | Greater ->printf ">"
    | Geq ->printf ">="
    | Or ->printf "|"
    | And ->printf "&"
    | Xor ->printf "^"
    | Not ->printf "~"
    | Lshift ->printf "<<"
    | Rshift -> printf">>"
    | Land -> printf"&&"
    | Lor -> printf"||" 
;;

let rec print_expr expr =
    match expr with
      Id(s) -> printf "ID=%s" s
    | Binop(e1, o, e2) -> printf "Binop="; print_expr e1 ; print_ops o;
    print_expr(e2)
    | Call(fname, args) -> printf "Call to function %s" fname; List.iter
    print_expr args
    | StructId(struct_type, var_name) -> printf "StructId: struct_type=%s, var_name=%s" struct_type var_name
    | Literal_int(i) -> printf "Int literal=%d" i
    | Literal_char(c) -> printf "Char literal=%c" c
    | Literal_float(f) -> printf "Float literal=%f" f
    | Literal_string(s) -> printf "String literal=%s" s
    | Literal_bool(b) -> printf "Bool literal=%B" b
    | Literal_int_a(i) -> List.iter (printf "%d, ") i
    | Literal_char_a(c) -> List.iter (printf "%c, ") c
    | Literal_float_a(f) -> List.iter (printf "%f, ") f
    | Literal_string_a(s) -> List.iter (printf "%s, ") s
    | Literal_bool_a(b) -> List.iter (printf "%B, ") b
    | _->printf "invalid expr"
;;

let rec print_stmt = function
      Block(s) -> List.iter print_stmt s 
    | Expr(e) -> print_expr e; printf "\n"
    | Assign(e1, e2) -> print_expr e1; print_expr e2; printf "\n" 
    | Return(e) -> printf "return "; print_expr e; printf "\n"
    | If(e1, ifbody, elsebody) -> printf "if("; print_expr e1; printf "){";
    print_stmt ifbody; printf "} else {"; print_stmt elsebody; printf "\n"
    | For(e1, e2, e3, body) -> printf "For("; print_expr e1; printf "; ";
    print_expr e2; printf "; "; print_expr e3; printf "){"; print_stmt body;
    printf "}\n"
    | While(e1, body) -> printf "While("; print_expr e1; printf "){"; print_stmt
    body; printf "}\n"
    | ForIn(e1, e2, body) -> printf "For("; print_expr e1; printf " in ";
    print_expr e2; printf "){"; print_stmt body; printf "}\n"
    | Continue -> printf "continue\n"
    | Break -> printf "break\n"
    | _-> printf "Statement error\n"
;;
let print_funcs fdecls = 
    let f func =
        printf "DEBUG: printing fdecl, fname=%s, gfunc=%B, blocksize=%d\n"
        func.fname func.isGfunc func.blocksize;
        printf "formals=<";
        print_vdecls func.formals;
        printf ">\n body=<";
        List.iter print_stmt func.body;
        printf ">\n";
    in List.iter f fdecls 
;;


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


let generate_checked_expr check_func expr env =
    if (check_func expr env) then expr, env
    else raise (BadExpressionError expr)

let exp_to_txt exp = 
    match exp with 
        Literal_int(i) -> string_of_int(i)
      | Literal_float(f) -> string_of_float(f)
      | Id(s) -> s 
      | _-> ""

let op_to_txt op = 
    match op with 
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divide -> "/"
    | Equal -> "=="
    | Greater -> ">"
    | Less -> "<"
    | Geq -> ">="
    | Leq -> "<="
    | Neq -> "!="
    | _-> ""

let rec args_to_txt arg_list str=
    match arg_list with
    | arg :: arg_tail -> args_to_txt arg_tail str ^ (exp_to_txt arg) ^ ", " 
    | [] -> 
            if (String.contains str ',') then
                (String.sub str 0 (String.length str - 2))
            else
                str
(* TODO: fix bug in string processing here*)

let generate_checked_binop check_binop binop env =
        check_binop binop env; 
        match binop with 
        Binop(e1, op , e2) -> Environment.append env [Text((exp_to_txt e1) ^ " "
        ^ (op_to_txt op) ^ " " ^ (exp_to_txt e2))]
        | _->  raise (BadExpressionError("binop"))

let generate_checked_array_access check_array_access array_expr env =
    check_array_access array_expr env;
    match array_expr with
    | ArrayAcc(e1, e2) -> Environment.append env [Text((exp_to_txt e1) ^ "[" ^
    (exp_to_txt e2) ^ "]")]
    | _-> raise (BadExpressionError("Array Access"))

let generate_checked_f_call check_f_call f_call env =
    check_f_call f_call env;
    match f_call with
    | Call(id, expressions) -> Environment.append env [Text(id ^ "(" ^
    (args_to_txt expressions "") ^ ")")];
    | _-> raise (BadExpressionError("Function Call"))

let generate_exp exp env = 
    match exp with
      Literal_int(i) -> Environment.append env [Text("/* Int
      */\n"); Text(string_of_int(i))]  
      | Literal_float(f) -> Environment.append env [Text("/* Float */");
      Text(string_of_float(f))] 
      | Literal_int_a(int_a) -> raise (NotImplementedError("int array literal"))
      | Literal_float_a(float_a) -> raise (NotImplementedError("float array literal"))
      | Id(s) -> Environment.append env [Text("/* Id */\n"); 
      Generator(generate_checked_id is_var_in_scope s )]  
      | Binop(_,_,_) -> Environment.append env [Text("/* Binop */\n");  Generator(generate_checked_binop Generator_utilities.expr_typeof exp )] 
      | Call(func_id, formals_list) -> Environment.append env [Text("/* Function
          Call */\n"); Generator(generate_checked_f_call
          Generator_utilities.expr_typeof exp)]
      | ArrayAcc(_, _) -> Environment.append env [Text("/* Array Access */\n"); 
      Generator(generate_checked_array_access Generator_utilities.expr_typeof exp)]
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

let generate_init vdecl exp env =
    if((Generator_utilities.vdecl_type vdecl) = (Generator_utilities.expr_typeof
    exp env)) then
        Environment.append env [Env(add_var vdecl.v_name
        (Generator_utilities.vdecl_type vdecl));
        Text(vdecl.v_type ^ " " ^ vdecl.v_name ^ " = "); Generator(generate_exp exp)]
    else
        raise(BadExpressionError("Assignment of incompatible types"))

let generate_assign id exp env =
    match id with
    | Id(a) -> if (is_var_in_scope a env) then
                    if(Generator_utilities.expr_typeof id = Generator_utilities.expr_typeof
                    exp) then
                        Environment.append env [Text(a ^ " =" ); Generator(generate_exp exp)]
                    else
                        raise(BadExpressionError("Assignment of incompatible types"))
               else
                    raise (BadExpressionError("assignment to undefined id"))
    | _-> raise (BadExpressionError("Invalid Assignment")) 

(* ------------------------------------------------------------------ *)		  
(* TODO combine-ify all of these functions *)
					  
let rec process_stmt_list stmt_list env = 
   match stmt_list with 
   stmt :: other_stmts -> Environment.append env [Generator(process_stmt
   stmt); Generator(process_stmt_list other_stmts)] 
   | []     -> "\n", env (* TODO this is a sanity check *) 
 and process_stmt stmt env =
   match stmt with 
   Vdecl(vdecl) -> Environment.append env [ Generator(process_vdecl vdecl);
   Text(";\n") ] 
   | Block(stmt_list) -> Environment.append env [ Generator(process_stmt_list
   stmt_list); Text(";\n") ] (* TODO check if we need braces/NewScope *) 
   | Expr(expr) -> Environment.append env [ Generator(generate_exp expr );
   Text(";\n") ]  
   | Assign(name, expr) -> Environment.append env [ Text("/* Assignment */\n");
   Generator(generate_assign name expr); Text(";\n")]
   | Return(expr) -> raise (NotImplementedError("expr")) 
   | Init(vdecl, expr) -> Environment.append env [ Text("/*Initialization*/\n");
   Generator(generate_init vdecl expr); Text(";\n")]
   | If(expr, bool_stmt, body) -> raise (NotImplementedError("if/else")) 
   | While(expr, stmt) -> raise (NotImplementedError("while")) 
   | ForIn(obj, container, stmt) -> raise (NotImplementedError("for in")) 
   | Continue -> raise (NotImplementedError("continue")) 
   | Break ->    raise (NotImplementedError("break"))
   | _ -> raise (NotImplementedError("Undefined type of expression")) 
 and process_vdecl vdecl env = 
   let v_datatype = Generator_utilities.str_to_type vdecl.v_type in 
   Environment.append env [Env(add_var vdecl.v_name v_datatype);
 	                   Text(vdecl.v_type ^ " " ^ vdecl.v_name)] 

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
(* Kernel invocation declarations                                     *)
(* ------------------------------------------------------------------ *)
(* If we have a gfunc declared as 

   > gfunc float[] my_gfunc(float[] arg1, float[] arg2, int arg3) 

   then our generated c code will call this function in the same
   way that cpu functions are called: 

   > result = my_gfunc(arg1, arg2, arg3, arg4)

   However, when my_gfunc is actually defined in the c file, its
   contents will be the boilerplate OpenCL code that invokes the
   kernel on the gpu.                                           

   At this point, all semantic checking has been completed, so we
   don't need to worry about checking the function map or anything
   like that.

   NOTES:
   - Because each kernel invocation function has its own C scope, we
     don't have to worry about variable name collision
   - The definitions of these functions will appear interspersed with
     cpu func definitions, but this will not interfere with namespace
     conventions. Essentially, the cpu code thinks it's calling
     another cpu function, but internally that cpu function is
     implemented as a gpu function

   - This method has the side-benefit that we don't have to process
     literals passed to functions differently

   TODO: find a good place for this comment block. *)
  
  
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
  | [vdecl] -> Environment.append env [Env((add_var vdecl.v_name
						    (Generator_utilities.vdecl_type vdecl)));
				       Text(vdecl.v_type ^ " " ^ vdecl.v_name)]

  | vdecl :: other_vdecls ->
     Environment.append env [Generator(generate_formals_vdecl vdecl);
			     Generator(generate_formals_vdecl_list other_vdecls)]
;;

(* kernel invocation ------------------------------------------------- *)
(* INVARIANTS:
   - the output array and input arrays of an individual gfunc MUST be
     the same size 
   - if there are non-array arguments, we rename them to __argn
   - the actual args list starts at arg2 (first 2 are reserved for
     size and output array*)
let generate_kernel_invocation_function fdecl env =
  let base_r_type = Generator_utilities.arr_type_str_to_base_type fdecl.r_type in
  let generate_cl_arg_list fdecl env =
    (* we need buffers for the output array and the input arrays 

       the size argument (arg0) will be __arr_len, and will always be
       called as such in the formals for this generated invocation
       function

       our job here is to assign __argn to either a cl_mem object for
       an array or simply the formal for primitives *)
    let rec generate_cl_args arg_n formals env =
      let generate_cl_arg arg_n formal env =
	if Generator_utilities.is_array_type formal.v_type then
	  (* create a cl memory buffer for array args *)
	  Environment.append env [Text(sprintf "cl_mem __arg%d = clCreateBuffer(" arg_n);
				  Text("__sheets_context,\n");
				  Text("CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,\n");
				  Text(sprintf "sizeof(%s) * __arr_len,\n" base_r_type);
				  Text(sprintf "(void *) %s," formal.v_name);
				  Text("&__cl_err);\n");
				  Text("CHECK_CL_ERROR(__cl_err, \"clCreateBuffer\");\n")]
	else
	  (* pass primitives directly *)
	  Environment.append env [Text(sprintf "%s __arg%d = %s;\n"
					       base_r_type arg_n formal.v_name)]
      in
      match formals with
	[] -> "", env
      | formal :: other_formals ->
	 Environment.append env [Generator(generate_cl_arg arg_n formal);
				 Generator(generate_cl_args (arg_n + 1) other_formals)]
    in
    (* we make a cl_mem buffer for the return array first *)
    Environment.append env [Text("cl_mem __arg1 = clCreateBuffer(");
			    Text("__sheets_context,\n");
			    Text("CL_MEM_WRITE_ONLY,\n");
			    Text(sprintf "sizeof(%s) * __arr_len,\n" base_r_type);
			    Text("&__cl_err);\n");
			    Text("CHECK_CL_ERROR(__cl_err, \"clCreateBuffer\");\n");
			    (* user-defined args start at 2 *)
			    Generator(generate_cl_args 2 fdecl.formals)] 
  in
  let generate_cl_enqueue_write_buffer_list fdecl env =
    let rec generate_cl_enqueue_write_buffers arg_n formals env =
      let generate_cl_enqueue_write_buffer arg_n formal env =
	if Generator_utilities.is_array_type formal.v_type then
	  Environment.append env [Text("CALL_CL_GUARDED(clEnqueueWriteBuffer,\n");
				  Text("(__sheets_queue,\n");
				  Text(sprintf "__arg%d,\n" arg_n);
				  Text("CL_TRUE,\n"); (* ensure blocking write *)
				  Text("0,\n");	      (* no offset *)
				  Text(sprintf "sizeof(%s) * __arr_len,\n" base_r_type);
				  Text(sprintf "(void *) %s,\n" formal.v_name);
				  Text("0,\n");
				  Text("NULL,\n");    (* no wait list *)
				  Text("NULL)");
				  Text(");\n")]
	else "", env (* no need to alloc buffer for primitives *)
      in
      match formals with
	[] -> "", env
      | formal :: other_formals ->
	 Environment.append env [Generator(generate_cl_enqueue_write_buffer arg_n formal);
				 Generator(generate_cl_enqueue_write_buffers
					     (arg_n + 1) other_formals)]
    in
    (* user-defined args start at 2 *)
    Environment.append env [Generator(generate_cl_enqueue_write_buffers 2 fdecl.formals)]
  in
  let generate_cl_set_kernel_args fdecl env =
    (* list of __argn vars *)
    let generate_arg_ns num_user_args env =
      let rec _helper num_arg_ns_left arg_n env =
	match num_arg_ns_left with
	  0 -> "", env
	| _ -> Environment.append env [Text(sprintf "__arg%d,\n" arg_n);
				       Generator(_helper (num_arg_ns_left - 1) (arg_n + 1))]
				  
      in
      Environment.append env [Generator(_helper num_user_args 2)]
    in
    Environment.append env [Text(sprintf "SET_%d_KERNEL_ARGS(" ((List.length fdecl.formals) + 2));
			    Text(sprintf "%s_compiled_kernel," fdecl.fname);
			    Text("__arr_len,\n");
			    Text("__arg1,\n");
			    Generator(generate_arg_ns (List.length fdecl.formals));
			    Text(");\n")]
  in
  let global_work_items = function
      (* provide a buffer for when block_size doesn't divide array
         size*)
      1 -> "__arr_len"
    | n -> "__arr_len /" ^ string_of_int n ^ " + 1"
  in
  let generate_cl_enqueue_nd_range_kernel fdecl env =
    Environment.append env [Text(sprintf "size_t *gdims = { %s };\n"
					 (global_work_items fdecl.blocksize));
			    Text("CALL_CL_GUARDED(clEnqueueNDRangeKernel,");
			    Text("(__sheets_queue,\n");
			    (* ocaml thinks this is type func_info *)
			    Text(sprintf "%s_compiled_kernel,\n" fdecl.fname); 
			    Text("1,\n"); (* only 1 dimensional array support *)
			    Text("0,\n"); (* 0 offset *)
			    Text("gdims,\n");
			    Text("NULL,\n");
			    Text("0,\n");
			    Text("NULL,\n");
			    Text("NULL)");
			    Text(");\n")]
  in		       
  let generate_cl_enqueue_read_buffer fdecl env =
    (* only one buffer to read, since there's only one output arg *)
    Environment.append env [Text(sprintf "%s *__out[__arr_len];\n" base_r_type);
			    Text("CALL_CL_GUARDED(clEnqueueReadBuffer,\n");
			    Text("(__sheets_queue,\n");
			    Text("CL_TRUE,\n"); (* blocking read *)
			    Text("0,\n");	(* 0 offset *)
			    Text(sprintf "sizeof(%s) * __arr_len,\n" base_r_type);
			    Text("(void *) __out,\n");
			    Text("0,\n"); (* empty wait queue *)
			    Text("NULL,\n");
			    Text("NULL)");
			    Text(");\n")]
  in
  let generate_cl_release_list fdecl env =
    let rec generate_cl_releases arg_n formals env =
      let generate_cl_release arg_n formal env =
	(* only release array args (those alloc-ed with
           clCreateBuffer) *)
	if Generator_utilities.is_array_type formal.v_type then
	  Environment.append env [Text("CALL_CL_GUARDED(");
				  Text("clReleaseMemObject, ");
				  Text(sprintf "(__arg%d)" arg_n);
				  Text(");\n")]
	else "", env
      in
      match formals with
	[] -> "", env
      | formal :: other_formals ->
	 Environment.append env [Text("CALL_CL_GUARDED("); (* always free output arr *)
				 Text("clReleaseMemObject, ");
				 Text("(__arg1)");
				 Text(");\n");
			         Generator(generate_cl_release arg_n formal);
				 Generator(generate_cl_releases (arg_n + 1) other_formals)]
    in				(* user args start at 2 *)
    Environment.append env [Generator(generate_cl_releases 2 fdecl.formals)]
  in
  let __arr_len = {
      v_type = "int";		(* TODO should we implement size_t *)
      v_name = "__arr_len";
      isConst = true;
      isStruct = false;
      a_size = -1;
    }
  in
  Environment.append env [Text(sprintf "%s %s("
				       (Generator_utilities.c_type_from_arr_type fdecl.r_type)
				       fdecl.fname);
		          Generator(generate_formals_vdecl_list (__arr_len :: fdecl.formals));
			  Text(")\n{\n");
			  Generator(generate_cl_arg_list fdecl);
			  Generator(generate_cl_enqueue_write_buffer_list fdecl);
			  Generator(generate_cl_set_kernel_args fdecl);
			  Generator(generate_cl_enqueue_nd_range_kernel fdecl);
			  Generator(generate_cl_enqueue_read_buffer fdecl);
			  Generator(generate_cl_release_list fdecl);
			  Text("return __out;\n");
			  Text("}\n")]

(* end kernel invocations -------------------------------------------- *)
(* ------------------------------------------------------------------- *)
let generate_func_formals_and_body stmt_list vdecl_list env = 
    Environment.append env [Generator(generate_formals_vdecl_list vdecl_list);
                            Text("){\n");
                            Generator(process_stmt_list (stmt_list));
                            Text("}\n"); ]

let rec generate_cpu_funcs fdecls env =
  let generate_cpu_func fdecl env =
    match fdecl.isGfunc with
      false ->
      let main_checked_name = function
	  "main" -> "snuggle"
	| other_name -> other_name
      in
      Environment.append env [Env(add_func
				    fdecl.fname (Generator_utilities.fdecl_to_func_info fdecl));
			      Text(sprintf "%s %s("
					   fdecl.r_type (main_checked_name fdecl.fname));
			      NewScope(generate_func_formals_and_body
					 fdecl.body fdecl.formals)]
                         
    | true ->
       Environment.append env [Env(add_gfunc (Generator_utilities.fdecl_to_func_info fdecl));
			       NewScope(generate_kernel_invocation_function fdecl)]
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
   - the incoming func_info struct is typechecked
   - the first argument of the __kernel is the size for the whole function 
   - the second argument of the __kernel is the output array *)

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
     (i.e. there won't be a gfunc called main)                        *)

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
    sprintf "CALL_CL_GUARDED(clReleaseKernel, (%s_compiled_kernel));\n" gf_info.id
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
