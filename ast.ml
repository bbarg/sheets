(* ast.ml
 * Abstract syntax tree for Sheets
 *
 * author: Benjamin Barg
 * Copyright 2014, Symposium Software *)

type op = ASSIGN | LOR | LAND | OR | XOR | AND | EQ | NEQ | LT | LEQ | GT | GEQ | LSHIFT | RSHIFT | PLUS | MINUS | TIMES | DIVIDE | MOD | NEG

type expr = 			(* Expressions *)
    Literal of int
  | Id of string
  | Assign of string * expr
	       
