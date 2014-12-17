(*
 * Sheets scanner
 *
 * Authors: Gabriel Blanco, Ruchir Khaitan
 * Copyright 2014, Symposium Software
 *)

{ open Parser;; }

let num = ['0'-'9']
let flt = '-'?num+ '.' num* | '.' num+

rule token = parse
(* Whitespace *)
| [' ' '\n' '\r'] { token lexbuf } 

(* Comments *)
| "#~" { comment lexbuf }

(* Punctuation *)
| '('        { LPAREN }  | ')'        { RPAREN }
| '{'        { LBRACE }  | '}'        { RBRACE }
| '['        { LBRACK }  | ']'        { RBRACK }
| ';'        { SEMI }    | ','        { COMMA }
| '.'        { PERIOD }  | ':'        { COLON }

(* Arithmetic Operators *)
| '+'        { PLUS }    | '-'        { MINUS }
| '*'        { TIMES }   | '/'        { DIVIDE }

(* Relational Operators *)
| "=="       { EQ }      | "!="       { NEQ }
| '<'        { LT }      | "<="       { LEQ }
| ">"        { GT }      | ">="       { GEQ }

(* Assignment Operator *)
| '='        { ASSIGN }

(* Conditional Keywords *)
| "if"       { IF }      | "else"     { ELSE } 

(* Loop Keywords*)
| "while"    { WHILE }   | "for"      { FOR }
| "break"    { BREAK}    | "continue" { CONTINUE }

(* Function Keywords *)
| "func"     { FUNC }    | "gfunc"    { GFUNC }
| "return"   { RETURN }

(* Type Keywords*)
| "int"      { INT }     | "float"    { FLOAT }
| "Block"    { BLOCK }

(* End-of-File *)
| eof { EOF }

(* Identifiers *)
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

(* Literals *)
| '-'?num+ as intlit { INT_LITERAL(int_of_string intlit) }
| flt      as fltlit { FLOAT_LITERAL(float_of_string fltlit) }

(* Throw Error for Invalid Token *)
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
| "~#" { token lexbuf }      (* End-of-comment *)
| _    { comment lexbuf }    (* Eat everything else *)
