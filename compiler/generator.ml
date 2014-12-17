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

let rec generate_type datatype env = 
  match datatype with 
  | Int ->        Environment.append env [Text("int")] 
  | Float ->      Environment.append env [Text("double")]
  | Array(t) ->   Environment.append env [ 
				       Generator(generate_type t); 
				       Text("*") (* Handling array types differently *)
				     ]

let generate_checked_id check_id id env = 
    if (check_id id env) then
        Environment.append env [Text(id)]
    else raise (VariableNotFound id)

(* TODO : This is an attempt to fix the lack of curried addition (wow curry yum) 
 * but I think what is happening is that one of the expressions on the side 
 * of a binop is a binop and that is getting ignored here 
 * note therefore that expressions need to be fully checked before being called 
 * to exp_to_txt
 *)
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

let rec exp_to_txt exp = 
    match exp with 
        Literal_int(i) -> string_of_int(i)
      | Literal_float(f) -> string_of_float(f)
      | Id(s) -> s
      | Binop(e1, op, e2) -> (exp_to_txt e1) ^ " " ^ (op_to_txt op) ^ " " ^ (exp_to_txt e2)
      | Literal_string(s) -> "\"" ^ s ^ "\""
      | _-> ""

let rec args_to_txt arg_list str=
    match arg_list with
     | [] -> 
            if(String.contains str ',') then
                String.sub str 0 (String.length str - 2)
            else
                str
    | arg :: arg_tail -> args_to_txt arg_tail (str ^ (exp_to_txt arg) ^ ", ") 

let generate_checked_binop check_binop binop env =
        check_binop binop env; 
        match binop with 
        Binop(e1, op , e2) ->   Environment.append env [
                                Text((exp_to_txt e1) ^ " "
                                ^ (op_to_txt op) ^ " " ^ 
                                (exp_to_txt e2))]
        | _->  raise (BadExpressionError("binop"))

let generate_checked_array_access check_array_access array_expr env =
    check_array_access array_expr env;
    match array_expr with
    | ArrayAcc(e1, e2) ->       Environment.append env [
                                Text((exp_to_txt e1) ^ "[" ^
                                (exp_to_txt e2) ^ "]")]
    | _-> raise (BadExpressionError("Array Access"))

let generate_checked_f_call check_f_call f_call env =
    check_f_call f_call env;
    match f_call with
    | Call(id, expressions) ->
       if env.on_gpu then
	 Environment.append env [Text(id ^ "(10, "
				      (* TODO size of array args *)
				      ^ (args_to_txt expressions "")
                                      ^ ")");]
       else
	 Environment.append env [Text(id ^ "(" 
				      ^ (args_to_txt expressions "")
				      ^ ")")]
    | _-> raise (BadExpressionError("Function Call"))

let rec print_int_array array_list str =
    match array_list with
    | [] -> 
            if(String.contains str ',') then
                String.sub str 0 (String.length str - 2)
            else
                str
    | head :: array_tail -> 
        print_int_array array_tail (str ^ (string_of_int head) ^ ", ")
;;

let rec print_float_array array_list str =    
    match array_list with
    | [] -> 
            if(String.contains str ',') then
                String.sub str 0 (String.length str - 2)
            else
                str
    | head :: array_tail -> 
        print_float_array array_tail (str ^ 
          (string_of_float head) ^ ", ")

;;


let rec generate_exp exp env = 
    match exp with
    | Literal_int(i) ->     Environment.append env [
                            Text(string_of_int(i))]  
    | Literal_float(f) ->   Environment.append env [
                            Text(string_of_float(f))] 
    | Literal_int_a(list_i) -> Environment.append env [
                                Text("{ ");
                                Text(print_int_array list_i "");
				Text("}")]
    | Literal_float_a(list_f) -> Environment.append env [
                                Text("{ ");
                                Text(print_float_array list_f "");
                                Text("}")]
    | Id(s) ->          Environment.append env [
                        Generator(generate_checked_id is_var_in_scope s )]  
    | Binop(_,_,_) ->   Environment.append env [
                        Generator(generate_checked_binop 
                            Generator_utilities.expr_typeof exp )] 
    | Call(func_id, formals_list) -> 
                        Environment.append env [
                        Generator(generate_checked_f_call
                            Generator_utilities.expr_typeof exp)]
    | ArrayAcc(_, _) -> Environment.append env [
                        Generator(generate_checked_array_access  
                            Generator_utilities.expr_typeof exp)]
    | BlockAcc(id, exp) ->Environment.append env [
                          Generator(generate_checked_block id exp)]
    | _-> raise (NotImplementedError("unsupported expression"))
and generate_checked_block id exp env =
    match id with
    | "start" -> Environment.append env [Text("__block_start")]
    | "end" -> Environment.append env [Text("__block_end")]
    | "out" -> 
            match exp with
            | Literal_int(a) -> 
                    if(a = -1) then
                        raise (BadExpressionError("Invalid block access"))
                    else
                        Environment.append env [Text("__out[" ^ (string_of_int a) ^ "]")]
            | _ -> Environment.append env [Text("__out"); Generator(generate_exp exp)]
    | _-> raise (BadExpressionError("Invalid block access"))
;;


let generate_init vdecl exp env =
    if((Generator_utilities.vdecl_type vdecl) = (Generator_utilities.expr_typeof
    exp env)) then 
            let v_type = Generator_utilities.vdecl_type vdecl in 

            match v_type with 
            Array(Int) ->  Environment.append env [Env(add_var vdecl.v_name (v_type));
                                                Text("int" ^ " " ^ vdecl.v_name ^ "[]  = ");   
                                                Generator(generate_exp exp);] 
           | Array(Float) ->  Environment.append env [Env(add_var vdecl.v_name (v_type));
                                                Text("float" ^ " " ^ vdecl.v_name ^ "[]  = ");   
                                                Generator(generate_exp exp);] 

          | _-> Environment.append env [
            Env(add_var vdecl.v_name (Generator_utilities.vdecl_type vdecl));
            Text((Generator_utilities.c_type_from_str vdecl.v_type) ^ " " ^ vdecl.v_name ^ " = ");   
            Generator(generate_exp exp)]
    else
        raise(BadExpressionError("Assignment of incompatible types"))

    (* TODO: make way to ensure return statements exist *)
    (* TODO: make way to ensure returned expression has been initialized *)
let generate_return exp env =
    let func_info = Environment.get_func_info env.current_function env in
    let return_type = func_info.return in
    let exp_type = Generator_utilities.expr_typeof exp env in
    if(exp_type = return_type) then
        Environment.append env [
        Text("return "); 
        Generator(generate_exp exp)]
    else
        raise(BadExpressionError("Bad return type"))
;;

let generate_assign id exp env =
    match id with
    | Id(a) -> if (is_var_in_scope a env) then
                  if(Generator_utilities.expr_typeof id env =
                     Generator_utilities.expr_typeof exp env) then
                        Environment.append env [
                        Text(a ^ " =" ); Generator(generate_exp exp)]
                  else
                        raise(BadExpressionError("Assignment of incompatible
                        types"))
               else
                    raise (BadExpressionError("assignment to undefined id"))
    | BlockAcc(s, expr) -> Environment.append env [Text("__out["); 
                                                  Generator(generate_exp expr); 
                                                  Text("] = "); 
                                                  Generator(generate_exp exp); 
                                                  Text(";");
                                                ]
    | _-> raise (BadExpressionError("Invalid Assignment")) 

(* ------------------------------------------------------------------ *)		  
let rec process_stmt_list stmt_list env = 
   match stmt_list with 
   stmt :: other_stmts -> Environment.append env [Generator(process_stmt
   stmt); Generator(process_stmt_list other_stmts)] 
   | []     -> Environment.append env [ Text("") ] 
 and process_stmt stmt env =
   match stmt with 
   Vdecl(vdecl) ->          Environment.append env [ 
                            Generator(process_vdecl vdecl);
                            Text(";\n") ] 
   | Block(stmt_list) ->    Environment.append env [ 
                            Generator(process_stmt_list stmt_list) ]
   | Expr(expr) ->          Environment.append env [ 
                            Generator(generate_exp expr );
                            Text(";\n") ]  
   | Assign(name, expr) ->  Environment.append env [ 
                            Generator(generate_assign name expr); 
                            Text(";\n")]
   | Return(expr) ->        Environment.append env [ 
                            Generator(generate_return expr); 
                            Text(";\n")] 
   | Init(vdecl, expr) ->   Environment.append env [ 
                            Generator(generate_init vdecl expr); 
                            Text(";\n")]
   | If(boolexpr, ifstmt, elsestmt) -> 
                            Environment.append env [ 
                            Generator(generate_if boolexpr ifstmt elsestmt)] 
   | While(expr, body) ->   Environment.append env [
                            NewScope(generate_while expr body)]
   | For(s1, e2, s3, body) -> 
                            Environment.append env [ 
                            NewScope(generate_for s1 e2 s3 body)]
   | ForIn(obj, container, stmt) -> raise (NotImplementedError("for in")) 
   | Continue ->            Environment.append env [
                            Text("continue;\n")]
   | Break ->               Environment.append env [
                            Text("break;\n")]
   | _ -> raise (NotImplementedError("Undefined type of expression")) 
 and process_vdecl vdecl env = 
   let v_datatype = Generator_utilities.str_to_type vdecl.v_type in
   Environment.append env [Env(add_var vdecl.v_name v_datatype);
 	                   Text((Generator_utilities.c_type_from_str vdecl.v_type) ^ " " ^ vdecl.v_name)] 

  and generate_while bool_expr body env =
      match bool_expr with 
      | Binop(e1, o, e2) ->
               match o with
                  | Equal -> append_while bool_expr body env
                  | Neq -> append_while bool_expr body env
                  | Greater -> append_while bool_expr body env 
                  | Less -> append_while bool_expr body env 
                  | Geq -> append_while bool_expr body env 
                  | Leq -> append_while bool_expr body env 
                  | _-> raise (BadExpressionError ("Binop is not boolean"))
      | _-> raise(BadExpressionError ("Conditional expression is not binop")) 
 and generate_for stmt1 bool_expr stmt2 body env =
      match bool_expr with
      | Binop(e1, o, e2) -> 
              match o with
                  | Equal -> append_for stmt1 bool_expr stmt2 body env
                  | Neq -> append_for stmt1 bool_expr stmt2 body env
                  | Greater -> append_for stmt1 bool_expr stmt2 body env 
                  | Less -> append_for stmt1 bool_expr stmt2 body env 
                  | Geq -> append_for stmt1 bool_expr stmt2 body env 
                  | Leq -> append_for stmt1 bool_expr stmt2 body env 
                  | _-> raise (BadExpressionError ("Binop is not boolean"))
      | _-> raise(BadExpressionError ("Conditional expression is not binop")) 
  and generate_if bool_expr ifbody elsebody env =
      match bool_expr with
      | Binop(e1, o, e2) -> 
              match o with
                  | Equal -> append_if_else bool_expr ifbody elsebody env
                  | Neq -> append_if_else bool_expr ifbody elsebody env
                  | Greater -> append_if_else bool_expr ifbody elsebody env 
                  | Less -> append_if_else bool_expr ifbody elsebody env 
                  | Geq -> append_if_else bool_expr ifbody elsebody env 
                  | Leq -> append_if_else bool_expr ifbody elsebody env 
                  | _-> raise (BadExpressionError ("Binop is not boolean"))
      | _-> raise(BadExpressionError ("Conditional expression is not binop")) 
and append_while bool_expr body env =
    Generator_utilities.expr_typeof bool_expr env;
    Environment.append env[Text("While("); Generator(generate_exp bool_expr);
    Text("){\n"); Generator(process_stmt body); Text("}\n")]
and append_if_else bool_exp ifbody elsebody env =
    Generator_utilities.expr_typeof bool_exp env;
    Environment.append env [Text("if("); Generator(generate_exp bool_exp);
    Text("){\n"); NewScope(process_stmt ifbody); Text("\n} else {\n");
    NewScope(process_stmt elsebody); Text("}\n")]

(* For loops have to have assignment, boolean expression, assignment 
 *)
and print_in_for_loop stmt first env = 
    match stmt with 
     Assign(name, expr) ->  if first then Environment.append env [ 
                            Generator(generate_assign name expr); 
                            Text(";")]
                            else  Environment.append env [ 
                            Generator(generate_assign name expr);]

      | _-> raise (BadExpressionError("Argument in for loop invalid")) 
   
and append_for stmt1 bool_exp stmt2 body env =
    Generator_utilities.expr_typeof bool_exp env;
    Environment.append env [Text("for("); Generator(print_in_for_loop stmt1 true );
    Generator(generate_exp bool_exp); Text("; "); 
    Generator(print_in_for_loop stmt2 false); Text("){\n"); Generator(process_stmt body);
    Text("}\n")]
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
				       Text((Generator_utilities.c_type_from_str vdecl.v_type) ^ " " ^ vdecl.v_name)]

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
				  Text(sprintf "(void *) %s,\n" formal.v_name);
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
			    Text("NULL,\n");
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
	| 1 -> Environment.append env [Text(sprintf "__arg%d\n" arg_n)]
	| _ -> Environment.append env [Text(sprintf "__arg%d,\n" arg_n);
				       Generator(_helper (num_arg_ns_left - 1) (arg_n + 1))]
				  
      in
      Environment.append env [Generator(_helper num_user_args 2)]
    in
    (* only need to add 1 because __arr_len is already in formals list *)
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
    Environment.append env [Text(sprintf "size_t gdims[] = { %s };\n"
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
    Environment.append env [Text(sprintf "%s *__out = (%s*) malloc(__arr_len * sizeof(%s));\n" base_r_type base_r_type base_r_type);
			    Text("CALL_CL_GUARDED(clEnqueueReadBuffer,\n");
			    Text("(__sheets_queue,\n");
			    Text("__out,\n");
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
				       (Generator_utilities.c_type_from_str fdecl.r_type)
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
                    Env(update_curr_func fdecl.fname);
			      Text(sprintf "%s %s("
					   fdecl.r_type (main_checked_name fdecl.fname));
			      NewScope(generate_func_formals_and_body
					 fdecl.body fdecl.formals)]
                         
    | true ->
       Environment.append env [Env(add_gfunc fdecl);
			       Env(add_func fdecl.fname (Generator_utilities.fdecl_to_func_info fdecl));
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

(* add the blocksize variables and then process the statment list
   *)
let rec generate_cl_kernel_body stmt_list fdecl env =
  Environment.append env
		     [Env(update_scope_add_var "__block_start" Int);
		      Env(update_scope_add_var "__block_end" Int);
		      Env(update_scope_add_var "_id" Int);
		      Text("const int _id = get_global_id(0);");
		      Text(sprintf "const int __block_start = _id * %d;"
				   fdecl.blocksize);
		      Text(sprintf "const int __block_end = _id * %d + %d;"
				   fdecl.blocksize
				   fdecl.blocksize);
		      Generator(process_stmt_list stmt_list);
		     ]

(* return a comma separated list of kernel formal declarations and
   adds the variables to the current scope *)
let rec generate_cl_kernel_vdecl_list vdecl_list env =
  let generate_cl_kernel_vdecl vdecl env =
    let c_type = Generator_utilities.c_type_from_str vdecl.v_type in
    let sheets_type = Generator_utilities.vdecl_type vdecl in
    Environment.append env [Env(update_scope_add_var vdecl.v_name sheets_type);
			    Text(sprintf "__global const %s %s" c_type vdecl.v_name)]
  in
  match vdecl_list with
    [] -> "", env
  | [vdecl] -> generate_cl_kernel_vdecl vdecl env
  | vdecl :: other_vdecls ->
     Environment.append env [Generator(generate_cl_kernel_vdecl vdecl);
			     Text(", ");
			     Generator(generate_cl_kernel_vdecl_list other_vdecls)]
  
let gfunc_to_cl_kernel_string gfdecl env =
  (* we have to reject all references to variables that aren't
     immediately in scope *)
  (* we're going to have to escape double-quotes when we write
     these string literals *)
  (* here we use KernelText instead of Text, which surrounds the
     string it takes in with quote marks and adds a newling at the end
     so that the text shows up in the generated c code as a multi-line
     string literal *)
  let base_r_type = Generator_utilities.c_type_from_str gfdecl.r_type in
  let sheets_r_type = Generator_utilities.str_to_type gfdecl.r_type in
  Environment.append env [
		       (* we have to manually modify scope because we're processing 
			  gfunc bodies separately from their declarations*)
		       Env(update_curr_func gfdecl.fname);
		       Env(update_on_gpu true);
		       Env(update_scope_add_var "__arr_len" Int);
		       Env(update_scope_add_var "__out" sheets_r_type);
		       Text("__kernel ");
		       Text(sprintf "void %s(__global const int __arr_len, __global %s__out,"
				    gfdecl.fname base_r_type);
		       Generator(generate_cl_kernel_vdecl_list gfdecl.formals);
		       Text(")");
		       Text("{");
		       Generator(generate_cl_kernel_body gfdecl.body gfdecl);
		       Text("}");
		       Env(update_on_gpu false);
		     ]

let gfunc_to_cl_kernel gfdecl env =
  Environment.append env [Text(sprintf "const char *%s_kernel_string =\n" gfdecl.fname);
			  (* we aren't ever changing the environment
			  above the gfunc's scope, but we need to
			  generate a new scope to parse the gfunc's
			  contents *)
			  NewScope(gfunc_to_cl_kernel_string gfdecl);
			  Text(";\n");
			  Text(sprintf "const char *%s_kernel_name = \"%s\";\n"
				       gfdecl.fname gfdecl.fname);
			  Text(sprintf "cl_kernel %s_compiled_kernel;\n" gfdecl.fname)]

let rec gfunc_list_to_cl_kernels gfdecl_list env =
  match gfdecl_list with
    [] -> "", env
  | gfdecl :: other_gfdecls ->
     Environment.append env [Generator(gfunc_to_cl_kernel gfdecl);
			     Generator(gfunc_list_to_cl_kernels other_gfdecls)]

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

let rec generate_compile_kernels gfdecl_list env =
  let generate_compile_kernel gfdecl =
    sprintf "%s_compiled_kernel = kernel_from_string(__sheets_context, %s_kernel_string, %s_kernel_name, SHEETS_KERNEL_COMPILE_OPTS);\n" gfdecl.fname gfdecl.fname gfdecl.fname
  in
  match gfdecl_list with
    [] -> "", env
  | gfdecl :: other_gfdecls ->
     Environment.append env [Text(generate_compile_kernel gfdecl);
			     Generator(generate_compile_kernels other_gfdecls)]
			
let rec generate_release_kernels gfdecl_list env =
  let generate_release_kernel gfdecl =
    sprintf "CALL_CL_GUARDED(clReleaseKernel, (%s_compiled_kernel));\n" gfdecl.fname
  in
  match gfdecl_list with
  [] -> "", env
  | gfdecl :: other_gfdecls ->
     Environment.append env [Text(generate_release_kernel gfdecl);
			     Generator(generate_release_kernels other_gfdecls)]

let generate_main env =
  Environment.append env [Text("int main()\n");
			  Text("{\n");
			  Text("create_context_on(SHEETS_PLAT_NAME, SHEETS_DEV_NAME, 0, &__sheets_context, &__sheets_queue, 0);\n");
			  Generator(generate_compile_kernels env.gfunc_list);
			  Text("snuggle();\n");
			  Generator(generate_release_kernels env.gfunc_list);
			  Text("CALL_CL_GUARDED(clReleaseCommandQueue, (__sheets_queue));\n");
			  Text("CALL_CL_GUARDED(clReleaseContext, (__sheets_context));\n");
			  Text("return 0;\n");
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
		^ "#include \"timing.h\"\n"
		^ "#include <CL/cl.h>\n\n"
                ^ "#define time_start() get_timestamp(&start)\n"
                ^ "#define time_end() get_timestamp(&end)\n" 
                ^ "timestamp_type start;\n"
                ^ "timestamp_type end;\n");
  print_string cl_kernels;
  print_string global_vdecls;
  print_string cpu_funcs;
  print_string main
;; 
