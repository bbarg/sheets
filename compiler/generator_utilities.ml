(* Utilities to parse expressions for generate.ml *)

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
    | Id(s) -> lookup_id_type (env, text)  
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
