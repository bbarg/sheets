(* utilities to parse expressions for generate.ml *)
open Ast;;
open Environment;;
open Printf;;

exception TypeError of string;;
exception NotImplementedError of string;;  
exception UnsupportedArrayTypeError;;

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
            | Array (Int) -> type1
            | Array (Float) -> type2
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
                (match int_expr with
                | Int -> Int
                | _-> raise (TypeError("Cannot access element in array with non-int datatype")))
        | Array ( Float) -> 
                (match int_expr with
                | Int -> Float
                | _-> raise (TypeError("Cannot access element in array with non-int datatype")))
        | _-> raise (TypeError("Cannot access element in non-array type"))
;;

(* Returns typeof block access 
 * TODO doesn't check expr type 
 *) 
let typeof_block_acc id env = 
    match id with 
   "start" -> Int 
   | "end" -> Int 
   | "out" -> (Environment.return_typeof_func env.current_function env)  
   | _-> raise (TypeError("Invalid block access"))
let rec expr_typeof expr env = 
    (*print_expr expr;*)
    match expr with 
     Literal_int(i) -> Int
    | Literal_float(f) -> Float 
    | Literal_int_a(i_a) -> Array( Int ) 
    | Literal_float_a(i_a) -> Array(Float) 
    | Id(s) -> Environment.typeof s env  
    | Binop(exp1, op, exp2) -> (eval_binop (expr_typeof exp1 env) (expr_typeof exp2 env) op) 
    | ArrayAcc(exp1, exp2) -> (eval_array_acc (expr_typeof exp1 env) (expr_typeof exp2 env) )
    | Call(func_id, expr_list ) -> (typeof_func_call func_id expr_list (Environment.get_func_args func_id env) env)
    | BlockAcc(id, _) -> (typeof_block_acc id env) 
    | _-> raise (NotImplementedError("Undefined type of expression"))

and typeof_func_call func_id expr_list arg_list env = 
    (* First make sure that all of the arguments are valid 
     * check all ids 
     * then return the type of the function 
     *)
   let rec check_expr_list expr_list arg_list = 
      match expr_list, arg_list with 
       | [],[] -> Environment.return_typeof_func func_id env 
       | expr1::other_exprs, arg1::other_args -> 
            if((expr_typeof expr1 env) != arg1 ) then
                raise (TypeError("Function arguments of incorrect type"))
            else
                check_expr_list other_exprs other_args
       | _-> raise (TypeError("Incorrect number of function arguments"))
    in 
      match func_id with 
      "printf" -> Int (* TODO this is a super hacky fix *) 
     | _ -> check_expr_list expr_list arg_list 
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
            _blocksize = fdecl.blocksize;
        }
;;
let arr_type_str_to_base_type = function
    "float[]" -> "double"
  | "int[]" -> "int"
  | "float[][]" -> "double"
  | "int[][]" -> "int"
  | _ -> raise UnsupportedArrayTypeError
;;
let c_type_from_str = function
    "int" -> "int"
  | "float" -> "double"
  | "float[]" -> "double *"
  | "int[]" -> "int *"
  | "float[][]" -> "double **"
  | "int[][]" -> "int **"
  | _ -> raise UnsupportedArrayTypeError
;;
let is_array_type = function
    "int" | "float" -> false
    | _ -> true
;;	     
