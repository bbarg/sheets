open Ast;;
open Scanner;;
open Parser;;
open Printf;; 

exception SyntaxError of int * int * string
  
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
    | Equal ->printf "="
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
    | Noexpr -> printf "Noexpr"
    | _->printf "invalid expr"
;;

let rec print_stmt = function
      Block(s) -> List.iter print_stmt s
    | Expr(e) -> print_expr e; printf "\n"
    | Assign(s, e) -> printf "%s=" s; print_expr e; printf "\n"
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
        printf ">\n locals=<";
        print_vdecls func.locals;
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

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = try
      Parser.program Scanner.token lexbuf
    with except ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let col = curr.Lexing.pos_cnum in
        let tok = Lexing.lexeme lexbuf in
        raise (SyntaxError (line, col, tok))
  in
  print_everything program
;;





