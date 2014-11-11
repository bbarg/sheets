(* ast.ml
 * Abstract syntax tree for Sheets
 * 
 * Throughout the AST there is are instances of pairs of types "thing"
 * and "gThing". In each instance, "thing" is a scalar type and
 * "gThing" is an array type.
 * 
 * author: Benjamin Barg
 * Copyright 2014, Symposium Software *)

type arrayExpr
type scalarExpr

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
