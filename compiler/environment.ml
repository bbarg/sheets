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
module StructMap   = Map.Make(String);;

exception NameAlreadyBoundError of string;;

(* Get variable, function, and struct maps from a level *)
let vars level = fst level;
let funcs level = snd level;
let structs level = trd level;
  
(* return a list containing one element: a tuple three empty maps *)
let empty () =
  [ (VariableMap.empty, FunctionMap.empty, StructMap.empty) ]
;;      

(* takes in the table list from the current scope level and
   returns a new table list with an empty_table appended.

   when descending a level in the generator, pass in `new_scope
   current_table` as the table argument *)
let new_scope env =
  empty @ env
;;		       

(* Membership Functions: each of these returns true if the name is
   unbound in all levels of the symbol table, and false if there is a
   binding *)
let unbound_var v_name env =
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

(* if the var, func, or struct id doesn't already exist, add it to the
map, otherwise throw an exception *)
let add_var var env =
  match (unbound_var var.v_name env) with
    true  -> match env with
	       current_level :: upper_levels
	       -> (VariableMap.add var.v_name var current_level)
		    :: upper_levels
    false -> raise (NameAlreadyBoundError(var.v_name))
;;
let add_func func env =
  match (unbound_func func.f_name env) with
    true  -> match env with
	       current_level :: upper_levels
	       -> (VariableMap.add func.f_name func current_level)
		    :: upper_levels
    false -> raise (NameAlreadyBoundError(func.f_name))
;;
let add_struct struc env =
  match (unbound_struct struc.s_name env) with
    true  -> match env with
	       current_level :: upper_levels
	       -> (VariableMap.add struc.s_name struc current_level)
		    :: upper_levels
    false -> raise (NameAlreadyBoundError(struc.s_name))
;;
