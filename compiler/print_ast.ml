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
    print_vdecls s_elements;
    printf ">\n";
  in List.iter f sdefs
;;

let print_everything (vdecls, sdefs, fdecls) =
    print_vdecls vdecls;
    print_sdefs sdefs
  (* print_decl_list fdecls; *)
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





