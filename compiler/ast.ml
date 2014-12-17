(*
 * Sheets Abstract Syntax Tree types
 *
 * Authors: Amelia Brunner, Gabriel Blanco
 * Copyright 2014, Symposium Software
 *)

type op = Plus    | Minus   | Times   | Divide  | Equal   | 
          Neq     | Less    | Leq     | Greater | Geq

type expr = 
    | Literal_int of int
    | Literal_int_a of int list
    | Literal_float of float    
    | Literal_float_a of float list
    | Id of string
    | Binop of expr * op * expr
    | Call of string * expr list
    | ArrayAcc of expr * expr
    (*   ArrayAcc(expr1,expr2) 
     *     expr1  : evaluates to an array, 
     *     expr2  : evaluates to the index of the element to be accessed *)
    | BlockAcc of string * expr
    (*   BlockAcc(string,expr)
     *     string : field of block (i.e. "start", "end", "out")
     *     expr   : index of block.out (Literal_int(0) otherwise *)

type vdecl = {
    v_type     : string;    (* PARSER *)
    v_name     : string;
    a_size     : int;
}

(* Statements *)
type stmt =
    | Vdecl of vdecl
    | Block of stmt list
    | Expr of expr
    | Assign of expr * expr
    | Return of expr
    | Init of vdecl * expr
    | If of expr * stmt * stmt 
    | For of stmt * expr * stmt * stmt
    | While of expr * stmt
    | Continue
    | Break

(* Function Declaration *)
type fdecl = {
    r_type     : string;
    fname      : string;
    formals    : vdecl list;    
    body       : stmt list;
    isGfunc    : bool;
    blocksize  : int;
}

type program = vdecl list * fdecl list

type datatype = 
    | Int
    | Float
    | Array of datatype
