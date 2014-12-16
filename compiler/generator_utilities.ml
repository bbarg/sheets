(* Utilities to parse expressions for generate.ml *)
open Ast;;
open Environment;;
open Printf;;

exception TypeError of string;;
exception NotImplementedError of string;;  
  
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

let rec print_expr = function
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

let print_everything (vdecls, sdefs, fdecls) =
    print_vdecls vdecls;
    print_sdefs sdefs;
    print_funcs fdecls
;;

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
    (*print_expr expr;*)
    match expr with 
     Literal_int(i) -> Int
    | Literal_float(f) -> Float 
    | Literal_int_a(i_a) -> Array( Int ) (* TODO: Check this *) 
    | Literal_float_a(i_a) -> Array(Float) 
    | Id(s) -> Environment.typeof s env  
    | Binop(exp1, op, exp2) -> (eval_binop (expr_typeof exp1 env) (expr_typeof exp2 env) op) 
    | ArrayAcc(exp1, exp2) -> (eval_array_acc (expr_typeof exp1 env) (expr_typeof exp2 env) )
    | Call(func_id, expr_list ) -> (typeof_func_call func_id expr_list (Environment.get_func_args func_id env) env)
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
    check_expr_list expr_list arg_list 
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
    "float[]" -> "float"
  | "int[]" -> "int"
  | "float[][]" -> "float"
  | "int[][]" -> "int"
  | _ -> raise UnsupportedArrayTypeError
;;
let c_type_from_arr_type = function
    "float[]" -> "float *"
  | "int[]" -> "int *"
  | "float[][]" -> "float **"
  | "int[][]" -> "int **"
  | _ -> raise UnsupportedArrayTypeError
;;
let is_array_type = function
    "int" | "float" -> false
    | _ -> true
;;	     
