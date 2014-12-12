open Scanner;;
open Parser;;
open Printf;; 


let print_vdecls vdecls = 
  let f (v_type, v_name, isConst, isStruct) =
    printf "DEBUG printing vdecl: v_type: %s, v_name: %s, isConst: %B, isStruct %B\n" v_type v_name isConst isStruct
  in List.map f vdecls
;;

let print_sdecls sdecls =
    let f (s_name, s_elements) =
        printf "%s: <" s_name;
        print_vdecls s_elements;
        printf(">\n");
    in List.map f sdecls
;;

let print_everything (vdecls, sdecls, fdecls) =
    print_vdecls vdecls;
    print_sdecls sdecls
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





