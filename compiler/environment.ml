(* environment.ml

   Scoped symbol table implementation for Sheets. We have three symbol
   tables:
   - variable names to vdecl objs
   - function names to fdecl objs

   Scoping will be implemented by a linked list to which we add when
   we go down in scope.

   Notes:

   - There should only ever be one entry per name in the symbol table *)

open Ast;;

module VariableMap = Map.Make(String);;
module FunctionMap = Map.Make(String);;
(* module StructMap   = Map.Make(String);; *) 

exception EmptyEnvironmentError;;
exception NameAlreadyBoundError of string;;
exception VariableNotFound of string;;
exception VariableAlreadyDeclared;; 
exception AlreadyDeclaredError;; 
exception FunctionNotDefinedError;;
exception ReservedWordError of string;;
  
type func_info  = {
	id : string; 
	on_gpu : bool;
	return : datatype; 
	args : datatype list;
	arg_names: string list;
	_blocksize : int;

}
(* Create a record type for env 
 *)

(* Record indicating what the current environment keeps track of *)
type env = {
    var_stack: datatype VariableMap.t list; 
    func_return_type_map: func_info FunctionMap.t; 
    current_function: string; 
    on_gpu: bool;
    gfunc_list: fdecl list;	(* we need to save the body of the
                                   gfunc so we can do kernel string
                                   generation *)
  } 
(* Types that can be returned by the generator as it modifies 
 * either the text of the generated code 
 * changes the environment as it is parsing the file 
 * or passes fuunctions to edit both code and environment 
 * either in the existing scope or in a new scope 
 *) 
type source = 
 | Text of string
 | Env of (env -> env) 
 | Generator of ( env -> (string * env))
 | NewScope of (env -> (string * env))


(* Create initializes an empty record for environment *)
let create = 
   {
       var_stack = VariableMap.empty::[];
       func_return_type_map = FunctionMap.empty; 
       current_function = ""; (* TODO maybe this needs a better convention *)
       on_gpu = false;
       gfunc_list = []; 
   }

(* Update gives a new env record with updated values 
 * of the record 
 *) 

let update v_stack f_map curr_f gpu g_list = 
    { 
        var_stack = v_stack; 
        func_return_type_map = f_map;
        current_function = curr_f; 
        on_gpu = gpu; 
        gfunc_list = g_list;
   }
(* Functions that let us modify only one 
 * variable in environment at a time 
 *)
let update_only_scope new_scope env = 
	update new_scope env.func_return_type_map env.current_function env.on_gpu env.gfunc_list 
let update_only_func new_func env = 
	update env.var_stack new_func env.current_function env.on_gpu env.gfunc_list 
let update_curr_func new_curr_func env = 
	update env.var_stack env.func_return_type_map new_curr_func env.on_gpu env.gfunc_list
let update_on_gpu gpu env = 
	update env.var_stack env.func_return_type_map env.current_function gpu env.gfunc_list
let update_gfunc_list g_list env = 
	update env.var_stack env.func_return_type_map env.current_function env.on_gpu g_list
(* Checks all scopes to see if variable has been declared *)
let is_var_in_scope id env = 
    let rec check_scopes scope_stack = 
        match scope_stack with 
            | [] -> false 
            | scope_level :: other_scope_levels ->
                  if VariableMap.mem id scope_level then 
                       true 
                  else check_scopes other_scope_levels 
    in check_scopes env.var_stack 
(* Checks all scopes to find variable and returns type if found
 * Raises exception if not found
 *)
let typeof id env = 
   let rec check_scopes scope_stack = 
        match scope_stack with 
           | [] -> raise (VariableNotFound id) 
           | scope_level :: other_scope_levels ->
              if VariableMap.mem id scope_level then 
                 VariableMap.find id scope_level 
              else 
                 check_scopes other_scope_levels 
       in check_scopes env.var_stack

(* Adds a variable to the topmost level of the variable stack
 * Does not check if variable is already in stack do elseware 
 * raises error if stack is an empty list 
 * returns an updated environment with an updated scope stack  
 *)
let update_scope_add_var id datatype env = 
    let old_scope, scope_tail = 
        ( match env.var_stack with 
            | old_scope :: scope_tail -> old_scope, scope_tail
            | [] -> raise EmptyEnvironmentError ) in 
    let new_scope = VariableMap.add id datatype old_scope in
       update_only_scope (new_scope::scope_tail) env    
    
(* Handles adding a variable to the current environment 
 * Takes an id : string , a datatype , and a 
 * (text: string, env : env) tuple and either raises an 
 * Empty error, a AlreadyDeclared error 
 * or it adds the variable to the current top scope 
 * with update_scope_add_var 
 * and returns a updated env 
 *)
  
let add_var id datatype env = 
    match env.var_stack with 
      | [] -> raise EmptyEnvironmentError 
      | scope_level :: scope_tail -> 
		if VariableMap.mem id scope_level then 
			raise VariableAlreadyDeclared
		else 
		update_scope_add_var id datatype env
     

(* adds a new empty variableMap to the top of var stack 
 * used to enter a subscope 
 * takes an env, returns an updated env 
 *)
    
let push_scope env =  
    update_only_scope (VariableMap.empty::env.var_stack) env
;;
(* removes a variableMap from  the top of var stack 
 * used to enter a subscope 
 * takes an env, returns an updated env 
 *)
    
let pop_scope env = 
   match env.var_stack with 
    | popped_scope::other_scopes -> 
        update_only_scope other_scopes env
    | [] -> raise EmptyEnvironmentError 


(* The following methods deal with handling the function map *) 

(* Takes a function id and checks if the function is defined
 * returns bool 
 *) 
let is_func_declared id env = 
   FunctionMap.mem id env.func_return_type_map 
(* Takes a function id and either returns the mapped function info * or raises an undefined function error 
 *)
let get_func_info id env = 
    if is_func_declared id env then 
        FunctionMap.find id env.func_return_type_map
    else 
        raise FunctionNotDefinedError

let get_func_args id env =
    (get_func_info id env).args


(* Inserts a new function to the function map 
 * and updates environment 
 * or raises a AlreadyDeclared error 
 *)
let add_func id finfo env = 
   if is_func_declared id env then 
	raise VariableAlreadyDeclared
   else
     (* XXX hack to convert sheets main to snuggle *)
    (* match id with
       "main" ->
       let env_with_snuggle =
	 update_only_func (FunctionMap.add "snuggle" finfo env.func_return_type_map) env
       in update_only_func (FunctionMap.add "main" finfo env.func_return_type_map) env
     | _ ->*)
	update_only_func (FunctionMap.add id finfo env.func_return_type_map) env	   
       
(* Returns the datatype of a function or 
 * raises a undefined error if the function is not defined 
 *) 
let return_typeof_func id env = 
    let f_info = 
	get_func_info id env in 
    f_info.return     

(* The append function - this is the 
 * running loop of the codegen step 
 * First, f is a function that takes a 
 * (text, environment) tuple and matches it 
 * with a component which is either 
 * some new text - gets appended to existing text
 * some modified environment - replaces existing 
 * environment 
 * some function named gen applied to 
 * the same scope - in which case gen is applied to 
 * the existing environment and gen returns a tuple 
 * of (string, env) and the text gets appended to the 
 * existing text, and the new env replaces the old env 
 * note that gen can be a function also with arguments 
 * given to it
 * then this function f is applied to component in the 
 * list of components always returning an updated 
 * (text, env) tuple that is passed to the next 
 * component in the list 
 *) 
(* Appends a new func_info for a gfunc to env's 
 * gdunc list and updates environment 
 * also makes sure it is a gfunc 
 *)

let rec check_gfunc_name_in_list glist gfunc_fdecl = 
    match glist with 
    [] -> false 
   | gfunc_fdecl :: rest_of_gfuncs -> if gfunc_fdecl.fname = gfunc_fdecl.fname then true
                                      else (check_gfunc_name_in_list rest_of_gfuncs gfunc_fdecl) 
let is_gfunc_declared gfunc_fdecl env = 
    if check_gfunc_name_in_list env.gfunc_list gfunc_fdecl then 
        raise (AlreadyDeclaredError) 
    else if is_func_declared gfunc_fdecl.fname env then 
        raise (AlreadyDeclaredError) 
    else false   
     

let add_gfunc gfunc_fdecl env = 
    (* TODO decide whether I want to check gfunc *)
    if (is_gfunc_declared gfunc_fdecl env) then 
      raise (AlreadyDeclaredError)
    else if (gfunc_fdecl.fname = "main") then
      raise (ReservedWordError("a gfunc cannot be the main method"))
    else 
       update_gfunc_list (gfunc_fdecl :: env.gfunc_list) env 


let append init_env components =  
   let f (text, env) component =  
      match component with 
       | Text(str) -> text ^ str, env 
       | Env (env_gen) -> let new_env = env_gen env in 
           text, new_env
       | Generator(gen) -> let new_str, new_env = gen env in 
           text ^ new_str, new_env 
       | NewScope(gen) -> 
         let new_str, new_env = gen (push_scope env) in 
               text ^ new_str, pop_scope new_env in 
   List.fold_left f("", init_env) components
