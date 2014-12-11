type op = Or | Xor | Not | Lshift | Rshift | 
          Plus | Minus | Times | Divide | Mod | Neg

type type_ = INT_LITERAL | CHAR_LITERAL | FLOAT_LITERAL | 
             INT_ARRAY_LITERAL | CHAR_ARRAY_LITERAL | FLOAT_ARRAY_LITERAL |
             STRING_LITERAL | STRING_ARRAY_LITERAL | BOOL_LITERAL | 
             BOOL_ARRAY_LITERAL

type expr = 
    Literal of type_
  | Noexpr
  | Id of string
  | Binop of expr * op * expr
  | Call of string * expr list

type stmt =		      (* statements that can occur in funcs *)
    Block of stmt list
  | Expr of expr
  | Assign of string * expr
  | Return of expr
  | If of expr * stmt * stmt 
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type vdecl = {
    _type     : string;	   (* PARSER *)
    name      : string;
    isConst   : bool;
    isStruct  : bool;
} 

type fdecl = {			   (* func declaration *)
    fname     : string;
    formals   : vdecl list;	   
    locals    : vdecl list;
    body      : stmt  list;
    gfunc     : bool;
    blocksize : int;
}

type sdecl = {			   (* struct declaration *)
    name      : string;
    elements  : vdecl list;
}

type program = vdecl list * sdecl list * fdecl list
(* program = global variables, global structs, funcs and gfuncs *)
