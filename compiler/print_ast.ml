open Scanner;;
open Parser;;
open Printf;; 


let print_vdecls vdecls = 
  let f (v_type, v_name, isConst, isStruct) =
    printf "DEBUG printing vdecl: v_type: %s, v_name: %s, isConst: %B, isStruct %B\n" v_type v_name isConst isStruct
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





