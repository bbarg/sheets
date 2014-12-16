(* Utilities to parse expressions for generate.ml *)
open Ast;;
open Environment;;

exception TypeError of string;;
exception NotImplementedError of string;;  

(*
let check_function_call f_call env =
    (* Check if function has been declared *)
    (* Check if args match what they should be *)
    match f_call with
    | Call(id, args) -> 

;;
*)

let eval_basic_binop type1 type2 = 
    if (type1 = type2) then 
        match type1 with
            | Int -> type1
            | Float -> type1
            | _-> raise (TypeError("Types not ID, int, or float"))
    else 
        raise (TypeError("Incompatible types"))
;;


let eval_binop type1 type2 op = 
    match op with 
    | Plus -> (eval_basic_binop type1 type2)
    | Minus -> (eval_basic_binop type1 type2)
    | Times -> (eval_basic_binop type1 type2)
    | Divide -> (eval_basic_binop type1 type2)
    | Equal -> (eval_basic_binop type1 type2)
    | Greater -> (eval_basic_binop type1 type2)
    | Less -> (eval_basic_binop type1 type2)
    | Geq  -> (eval_basic_binop type1 type2)
    | Leq  -> (eval_basic_binop type1 type2)
    | Neq    -> (eval_basic_binop type1 type2) 
    | _->      raise (TypeError("Incompatible types"))


let eval_array_acc array_ int_expr =
    match array_ with
        | Array ( Int) -> 
                match int_expr with
                | Int -> array_
                | _-> raise (TypeError("Cannot access element in array with non-int datatype"))
        | Array ( Float) -> 
                match int_expr with
                | Int -> array_
                | _-> raise (TypeError("Cannot access element in array with non-int datatype"))
        | _-> raise (TypeError("Cannot access element in non-array type"))
;;

let rec expr_typeof expr env = 
    match expr with 
     Literal_int(i) -> Int
    | Literal_float(f) -> Float 
    | Literal_int_a(i_a) -> Array( Int ) (* TODO: Check this *) 
    | Literal_float_a(i_a) -> Array(Float) 
    | Id(s) -> Environment.typeof s env  
    | Binop(exp1, op, exp2) -> (eval_binop (expr_typeof exp1 env)
    (expr_typeof exp2 env) op) 
    | ArrayAcc(exp1, exp2) -> (eval_array_acc (expr_typeof exp1 env)
    (expr_typeof exp2 env) )
    (*    | Call(func_id, expr_list ) -> (typeof_func_call func_id expr_list env) *)
    | _-> raise (NotImplementedError("Undefined type of expression"))
(* TODO : What do we need  
 *)
(*
and typeof_func_call func_id expr_list env = 
    (* First make sure that all of the arguments are valid 
     * check all ids 
     * then return the type of the function 
     *)
   let check_expr_list expr_list = 
      match expr_list with 
       [] -> Environment.return_typeof_func func_id env 
       | expr:: other_exprs -> let check_expr exp = 
             match exp with 
              Id(s) -> if Environment.is_var_in_scope expr env then 
                         check_expr_list other_exprs 
             | _-> check_expr_list other_exprs in 
    check_expr expr in 
  check_expr_list expr_list; 
;;
*)

                 
                        
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
let vdecl_type vdecl = 
    str_to_type vdecl.v_type
;;
let rec vdecl_list_to_type_list vdecl_list = 
    match vdecl_list with 
        vdecl::rest_of_vdecls -> (vdecl_type vdecl)::(vdecl_list_to_type_list rest_of_vdecls )
       | [] -> [] 
;;
let rec vdecl_list_to_string_list vdecl_list = 
    match vdecl_list with 
        vdecl::rest_of_vdecls -> (vdecl.v_name)::(vdecl_list_to_string_list rest_of_vdecls )
       | [] -> [] 
;;
let fdecl_to_func_info fdecl = 
        { 
            id = fdecl.fname;
            on_gpu = fdecl.isGfunc; 
            return = str_to_type fdecl.r_type; 
            args = vdecl_list_to_type_list fdecl.formals;       
            arg_names = vdecl_list_to_string_list fdecl.formals;       
        }
;;
