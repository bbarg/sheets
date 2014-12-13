(* Utilities to parse expressions for generate.ml *)

(* Utilities for type checking *) 

(* Takes an environment and text tuple and an id of a variable, and returns its type as a string. If the variable does not exist it raises an error *)

let lookup_id_type (env, text) id = "TODO" ;;


(* Takes an environment and text tuple and name of a function, and returns its return type as a string. If the function does not exist it raises an error *)

let lookup_func_type (env, text) f = "TODO";;
(* Takes an expression and the corresponding environment and current text and evaluates the type of the expression, returning a string corresponding to the type of the expression *)
exception TypeError of string;;

let rec typeof (env, text) expr = 
    match expr with 
     Literal_int(i) -> "int"
    | Literal_char(c) -> "char"
    | Literal_float(f) -> "float" 
    | Literal_string(s) -> "string"
    | Literal_bool(b) -> "bool"
    | Literal_int_a(i_a) -> "int[]" 
    | Literal_char_a(i_a) -> "char[]" 
    | Literal_float_a(i_a) -> "float[]" 
    | Literal_string_a(i_a) -> "string[]"
    | Literal_bool_a (s_a) ->  "bool[]"
    | Id(s) -> lookup_id_type (env, text) s 
    | Binop(exp1, op, exp2) -> eval_binop (env, text) (typeof(env, text) exp1) (typeof (env, text) exp2) op 
    | Call(fname, exp_list) -> lookup_func_type (env, text) fname
    | StructId(s, elem) -> lookup_struct_mem_type (env, text) s elem 
    | _-> raise (Generator.NotImplementedError("Undefined type of expression"))
;;


let process_expr (env, tex) expr = (env, tex);;

(* take in the current environment and text and return the updated
   environment and text with the c code for the assignment
   raises TypeError if the type of the variable and expression do not
   match
 *)
let process_assign (env, text) name expr =
  raise Generator.NotImplementedError("utilities: process_assign")
(* TODO *)
;;

let process_return (env, text) expr =
  
;;
