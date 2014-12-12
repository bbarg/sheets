open Scanner;;
open Parser;;

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
      print_string "uh oh..."
  in
  print_everything program;;





