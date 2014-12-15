(* environment.ml

   Scoped symbol table implementation for Sheets. We have three symbol
   tables:
   - variable names to vdecl objs
   - function names to fdecl objs
   - struct names to sdef objs

   Scoping will be implemented by a linked list to which we add when
   we go down in scope.

   Notes:

   - When a new scope is created, the creator level does not get a
     modified symbol table back when the scope returns (a kind of
     funny way to implement a stack; we don't actually explicitly say
     to pop things off, we just don't save anything that is added when
     we go down in scope) 
   - There should only ever be one entry per name in the symbol table *)

open Three_tuple;;
open Ast;;

module VariableMap = Map.Make(String);;
module FunctionMap = Map.Make(String);;
(* module StructMap   = Map.Make(String);; *) 

exception EmptyEnvironmentError;;
exception NameAlreadyBoundError of string;;
exception VariableNotFound of string;;
exception VariableAlreadyDeclared;; 
exception FunctionNotDefinedError;; 
(* Get variable, function, and struct maps from a level *)
(* let vars level = first level;;   *)
(* let funcs level = second level;; *)
(* let structs level = third level;; *) 
  
(* START OF NEW DEFINITION for ENV *)
(* let create = *)
(*    { *)
(*      var_map_stack = VariableMap.empty :: []; *)
(*      func_map = FunctionMap.empty; *)
(*      struct_map = StructMap.empty; *)
(*      containing_func = "";	(\* initialize as null fdecl *\) *)
(*    }  *)
(* ;; *)

(* return a list containing one element: a tuple of three empty maps *)
(* let empty () =
  [ (VariableMap.empty, FunctionMap.empty, StructMap.empty) ] 
;; *)

(* takes in the table list from the current scope level and
   returns a new table list with an empty_table appended.

   when descending a level in the generator, pass in `new_scope
   current_table` as the table argument *)
(* let new_scope env =
  empty() @ env
;; *)		       

(* Membership Functions: each of these returns true if the name is
   unbound in all levels of the symbol table, and false if there is a
   binding *)
(* let unbound_var v_name env =
  let name_not_in_level (vars, _, _) =
    not (VariableMap.mem v_name vars)
  in
  List.for_all name_not_in_level env
;;
let unbound_func f_name env =
  let name_not_in_level (_, funcs, _) =
    not (FunctionMap.mem f_name funcs)
  in
  List.for_all name_not_in_level env
;;
let unbound_struct s_name env = 
  let name_not_in_level (_, _, structs) =
    not (StructMap.mem s_name structs)
  in
  List.for_all name_not_in_level env
;;
*)
(* if the var, func, or struct id doesn't already exist, add it to the
map, otherwise throw an exception *)
(* let add_var var env =
  let _add = function
      [] -> raise EmptyEnvironmentError
    | current_level :: upper_levels
      -> (VariableMap.add var.v_name var (vars current_level),
	  funcs current_level,
	  structs current_level) :: upper_levels
  in      
  match (unbound_var var.v_name env) with
    true -> _add env
  | false -> raise (NameAlreadyBoundError(var.v_name))
;;
let add_func func env =
  let _add = function
      [] -> raise EmptyEnvironmentError
    | current_level :: upper_levels
      -> (vars current_level,
	  FunctionMap.add func.fname func (funcs current_level),
	  structs current_level) :: upper_levels
  in      
  match (unbound_func func.fname env) with
    true -> _add env
  | false -> raise (NameAlreadyBoundError(func.fname))
;;
let add_struct struc env =
  let _add = function
      [] -> raise EmptyEnvironmentError
    | current_level :: upper_levels
      -> (vars current_level,
	  funcs current_level,
	  FunctionMap.add struc.s_name struc (structs current_level)) :: upper_levels
  in      
  match (unbound_struct struc.s_name env) with
    true -> _add env
  | false -> raise (NameAlreadyBoundError(struc.s_name))
;;
*)
type func_info  = {
	id : string; 
	on_gpu : bool;
	return : datatype; 
	args : datatype list; 

}
(* Create a record type for env 
 * TODO why do I need to make it a parameterized record
 *)

(* Record indicating what the current environment keeps track of *)
type env = {
   var_stack: datatype VariableMap.t list; 
   func_return_type_map: func_info FunctionMap.t; 
   current_function: string; 
   on_gpu: bool; 

} 
(* TODO understand this next part better - 
 * Seems like a polymorphic function/functor 
 * mapping to either Text string or a tuple of 
 * (some type, string ) 
 *) 
type source = 
 | Text of string 
 | Generator of ( env -> (string * env))
 | NewScopeGenerator of (env -> (string * env))


(* Create initializes an empty record for environment *)
let create = 
   {
       var_stack = VariableMap.empty::[];
       func_return_type_map = FunctionMap.empty; 
       current_function = ""; (* TODO maybe this needs a better convention *)
       on_gpu = false; 

   
  }

(* Update gives a new env record with updated values 
 * TODO maybe we can make something to just update a part 
 * of the record 
 *) 

let update v_stack f_map curr_f gpu = 
    { 
        var_stack = v_stack; 
        func_return_type_map = f_map;
        current_function = curr_f; 
        on_gpu = gpu; 
   }

let update_only_scope new_scope env = 
	update new_scope env.func_return_type_map env.current_function env.on_gpu 
let update_only_func new_func env = 
	update env.var_stack new_func env.current_function env.on_gpu 
let update_curr_func new_curr_func env = 
	update env.var_stack env.func_return_type_map new_curr_func env.on_gpu 
let update_on_gpu gpu env = 
	update env.var_stack env.func_return_type_map env.current_function gpu 
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
    (* update (new_scope:: scope_tail) env.func_return_type_map env.current_function env.on_gpu *) 
    

(* Handles adding a variable to the current environment 
 * Takes an id : string , a datatype , and a 
 * (text: string, env : env) tuple and either raises an 
 * Empty error, a AlreadyDeclared error 
 * or it adds the variable to the current top scope 
 * with update_scope_add_var 
 * and returns a (text, updated_env) tuple 
 *)
  
let add_var id datatype (text, env) = 
    match env.var_stack with 
      | [] -> raise EmptyEnvironmentError 
      | scope_level :: scope_tail -> 
		if VariableMap.mem id scope_level then 
			raise VariableAlreadyDeclared
		else 
		(text, update_scope_add_var id datatype env)
     

(* adds a new empty variableMap to the top of var stack 
 * used to enter a subscope 
 * takes an env, returns an updated env 
 *)
(* update (VariableMap.empty:: env.var_stack) env.func_return_type_map env.current_function env.on_gpu *) 
    
let push_scope env =  
    update_only_scope (VariableMap.empty::env.var_stack) env
;;
(* removes a variableMap from  the top of var stack 
 * used to enter a subscope 
 * takes an env, returns an updated env 
 *)
(*update other_scopes env.func_return_type_map env.current_function env.on_gpu *)
    
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
(* Inserts a new function to the function map 
 * and updates environment 
 * or raises a AlreadyDeclared error 
 *)
let add_func id finfo env = 
   if is_func_declared id env then 
	raise VariableAlreadyDeclared
   else 
       update_only_func (FunctionMap.add id finfo env.func_return_type_map ) env 
(* Returns the datatype of a function or 
 * raises a undefined error if the function is not defined 
 *) 
let return_typeof_func id env = 
    let f_info = 
	get_func_info id env in 
    f_info.return     

(* The combine function - this is the heart of the 
 * environment class 
 *) 


let combine init_env components =  
   let f (text, env) component =  
      match component with 
       | Text(str) -> text ^ str, env 
       | Generator(gen) -> let new_str, new_env = gen env in 
           text ^ new_str, new_env 
       | NewScopeGenerator(gen) -> 
         let new_str, new_env = gen (push_scope env) in 
               text ^ new_str, pop_scope new_env in 
    List.fold_left f("", init_env) components
