(* ast.ml
 * Abstract syntax tree for Sheets
 * 
 * Throughout the AST there are instances of pairs of types "thing"
 * and "gThing". In each instance, "thing" is a scalar type and
 * "gThing" is an array type.
 * 
 * author: Benjamin Barg
 * Copyright 2014, Symposium Software *)

(* List of types that parse solely as tokens:
   - varType
   - arrayLiteral
   - arrayOp
   - scalarOp
 *)

type scalar = 
  (* all the literal types *)
  | Id of string 		(* scalar-valued variables *)

(* TODO is it an issue that arrayExpr and scalarExpr will have really
similar rule trees? *)
type arrayExpr = 
    Literal of arrayLiteral
  | Id of string
  (* | Assign of string * arrayExpr *) (* TODO are we supporting? *)
  | Binop of arrayExpr * arrayOp * arrayExpr
  | Binop of arrayExpr * arrayOp * scalar
  | Call of string * expr list	

type scalarExpr =
    Scalar of scalar
  (* | Assign of string * scalarExpr *) (* TODO are we supporting? *)
  | Binop of scalarExpr * scalarOp * scalarExpr
  | Call of string * expr list

(* NOTE somtimes need to refer to all types of expressions, sometimes
 * either or *)
type expr = arrayExpr | scalarExpr

type scope = {
    vLocals   : vDecl list;
    sLocals   : sDecl list;
    body      : stmt  list;
}

type stmt =		      (* statements that can occur in funcs *)
    Scope of scope
  | Assign of string * expr
  | Return of expr
  | If of scalarExpr * scope * scope 	(* TODO deal with "elif" and "else" *)
  | For of expr * scalarExpr * expr * scope (* TODO deal with boolean? *)
  | While of expr * scope
  
type gStmt =		     (* statements that can occur in gfuncs *)
    Scope of scope
  | Assign of string * expr
  | If of scalarExpr * scope * scope
  | For of expr * scalarExpr * expr * scope
  | While of expr * scope

type vDecl = {
    _type     : varType;	   (* PARSER *)
    name      : string;
    isConst   : bool;
} 

type fDecl = {			   (* func declaration *)
    name      : string;
    formals   : vDecl list;	   
    locals    : vDecl list;
    body      : stmt  list;
}

type gDecl  = {			   (* gfunc declaration *)
    name      : string;
    formals   : vDecl list;	   
    locals    : vDecl list;
    body      : gStmt list;
    blockSize : int; 
}

type sDecl = {			   (* struct declaration *)
    name      : string;
    elements  : vDecl list;
}

type sheet = vDecl list * sDecl list * fDecl list * gDecl list
