(* ast.ml
 * Abstract syntax tree for Sheets
 *
 * author: Benjamin Barg
 * Copyright 2014, Symposium Software *)

type op = ASSIGN | LOR | LAND | OR | XOR | AND | EQ | NEQ | LT | LEQ | GT | GEQ | LSHIFT | RSHIFT | PLUS | MINUS | TIMES | DIVIDE | MOD | NEG

(* List of types we need *)
type program = {}
type func_decl = {}
type gfunc_decl = {}		(* MAYBE *)
type array = {}			(* TODO how to deal with differently typed arrays *)
type 

(* type scalar =  *)
(*     Literal of int *)
(*   | Literal of float *)

(* type expr = 			(\* Expressions *\) *)
(*   Literal of int 		(\* 35 *\) *)
(*   | Literal of string		(\* "string thing" *\) *)
(*   | Noexpr			(\* for (;;) *\) *)
(*   | Id of string		(\* foo *\) *)
(*   | Assign of string * expr	(\* foo = 35 *\) *)
(*   | Binop of expr * op * expr 	(\* a + b *\) *)
(*   | Call of string * expr list	(\* foo(2, 3 *\) *)

(* type stmt =			     (\* Statements *\) *)
(*     Block of stmt list		     (\* { ... } *\) *)
(*   | Expr of expr		     (\* foo = bar + 2 *\) *)
(*   | Return of epr		     (\* return 35 *\) *)
(*   | If of expr * stmt * stmt	     (\* if (foo == 35):{} else {} *\) *)
(*   | For of expr * expr * expr * stmt (\* for (i=0;i<5;i=i+1) { ... } *\) *)
(*   | While of expr * stmt	     (\* while (i<5):{ i = i + 2 } *\) *)
			  
(* type func_decl = { *)
(*     is_gfunc : bool;		(\* Whether function is a gfunc *\) *)
(*     formals : string list;	(\* Formal argument names *\) *)
(*   } *)

(* type program = string list * func_decl lit *)
