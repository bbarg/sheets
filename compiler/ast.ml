type op = Lor      | Land    | Or      | Xor     | And     |
          Neq      | Less    | Leq     | Greater | Geq     |
          Plus     | Minus   | Times   | Divide  | Mod     |  
          Equal    | Lshift  | Rshift

type unary_op = Neg| Not

type datatype =
  | INT              
  | LONG
  | FLOAT     
  | DOUBLE
  | CHAR      
  | STRING
  | BOOL
  | StructType of ident
  | ArrayType of datatype

type ident = Id of string

type lvalue =
  | Variable of ident
  | ArrayElem of ident * expr list
and expr = 
  | Literal_int of int
  | Literal_char of char
  | Literal_float of float
  | Literal_string of string
  | Literal_bool of bool
  | Literal_array of expr list  
  (**)
  | BinaryOp of expr * op * expr
  | UnaryOp of unary_op * expr
  | StructAccess of ident * ident
  | FunctionCall of ident * expr list
  | StructId of string * string 
  (* StructId: string1=name of struct var, string2=name of element to be accessed *)
  | ArrayAccess of ident * expr
  (* ArrayAcc: expr1=expression that evaluates to an array, expr2=expression
   * that evaluates to the index in the array to be accessed *)

type decl =
  | AssigningDecl of ident * expr
  | PrimitiveDecl of datatype * ident
  | ArrayDecl of datatype * ident * expr list

type vdecl = {
    v_type    : string;	   (* PARSER *)
    v_name    : string;
    isConst   : bool;
    isStruct  : bool;
    a_size    : expr;
}

type stmt =		      (* statements that can occur in funcs *)
  | Vdecl of vdecl
  | Block of stmt list
  | Expr of expr
  | Assign of lval * expr
  | Return of expr
  | Init of vdecl * expr
  | If of expr * stmt * stmt 
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | ForIn of expr * expr * stmt
  | Continue
  | Break

type fdecl = {			   (* func declaration *)
    r_type    : string;
    r_struct  : bool;
    fname     : string;
    formals   : vdecl list;	   
    body      : stmt list;
    isGfunc   : bool;
    blocksize : int;
}

type sdef = {			     (* struct definition *)
    s_name     : string;
    s_elements : vdecl list;
}

(* program = global variables, global structs & functions *)
type program = vdecl list * sdef list * fdecl list

let type_of_string = function
  | "int"     -> INT              
  | "long"    -> LONG
  | "float"   -> FLOAT     
  | "double"  -> DOUBLE
  | "char"    -> CHAR      
  | "String"  -> STRING
  | "boolean" -> BOOL
