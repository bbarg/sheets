(* Utilities to parse expressions for generate.ml *)
open Ast;;
open Environment;;

exception TypeError of string;;
exception NotImplementedError of string;;  

let eval_basic_binop type1 type2 = 
    if (type1 = type2) then 
        type1
    else 
        raise (TypeError("Incompatible types"))
;;
let eval_binop type1 type2 op = 
    match op with 
    | Plus -> eval_basic_binop type1 type2
    | Minus -> eval_basic_binop type1 type2
    | Times -> eval_basic_binop type1 type2
    | Divide -> eval_basic_binop type1 type2
    | Equal -> eval_basic_binop type1 type2
    | Greater -> eval_basic_binop type1 type2
    | Less -> eval_basic_binop type1 type2
    | Geq  -> eval_basic_binop type1 type2
    | Leq  -> eval_basic_binop type1 type2
    | Neq    -> eval_basic_binop type1 type2 
    | _->      raise (TypeError("Incompatible types"))


let rec expr_typeof expr env = 
    match expr with 
     Literal_int(i) -> Int
    | Literal_float(f) -> Float 
    | Literal_int_a(i_a) -> Array( Int ) (* TODO: Check this *) 
    | Literal_float_a(i_a) -> Array(Float) 
    | Id(s) -> Environment.typeof s env  
(*    | Binop(exp1, op, exp2) -> eval_binop expr_typeof exp1 env expr_typeof exp2 env op env *)
    | Call(func_id, _ ) -> Environment.return_typeof_func func_id env 
    | _-> raise (NotImplementedError("Undefined type of expression"))
;;


let str_to_type str = 
        match str with 
        "int" -> Int 
       | "float" -> Float
       | "int[]" -> Array(Int) (* TODO Enumerate other types *)  
       | "float[]" -> Array(Float) 
       |  _-> raise (NotImplementedError("Unrecognized type " ^ str))

let rec typecheck_stmt stmt env = true;; (* TODO *)
let rec typecheck_stmt_list stmt_list env = 
    match stmt_list with 
    [] -> true 
    | stmt :: rest_of_stmts -> typecheck_stmt stmt env; 
                               typecheck_stmt_list rest_of_stmts env

;;
