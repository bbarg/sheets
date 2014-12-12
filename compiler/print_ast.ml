open Scanner;;
open Parser;;

exception SyntaxError of int * int * string
  
let print_vdecls vdecls = 
  let f (_type, name, isConst, isStruct) =
    Printf.printf "%s, %s\n" _type name
  in List.map f vdecls
;;
	   
let print_everything (vdecls, sdecls, fdecls) =
  print_vdecls vdecls
  (* print_decl_list sdecls; *)
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





