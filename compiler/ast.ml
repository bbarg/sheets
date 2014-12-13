type op = Lor      | Land    | Or      | Xor     | Not     | And     |
          Equal    | Neq     | Less    | Leq     | Greater | Geq     |
          Plus     | Minus   | Times   | Divide  | Mod     | Neg     | 
          Lshift   | Rshift

type expr = 
  | Literal_int of int
  | Literal_char of char
  | Literal_float of float
  | Literal_string of string
  | Literal_bool of bool
  | Literal_int_a of int list
  | Literal_char_a of char list
  | Literal_float_a of float list
  | Literal_string_a of string list
  | Literal_bool_a of bool list
  | Id of string
  | Binop of expr * op * expr
  | Call of string * expr list
  | StructId of string * string 
  (* StructId: string1=name of struct var, string2=name of element to be accessed *)
  | ArrayAcc of expr * expr
  (* ArrayAcc: expr1=expression that evaluates to an array, expr2=expression
   * that evaluates to the index in the array to be accessed *)

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
  | Assign of expr * expr
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
    body      : stmt  list;
    isGfunc   : bool;
    blocksize : int;
}

type sdef = {			     (* struct definition *)
    s_name     : string;
    s_elements : vdecl list;
}

type program = vdecl list * sdef list * fdecl list
(* program = global variables, global structs, functions *)
