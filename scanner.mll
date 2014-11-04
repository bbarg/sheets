{ open Parser } (* Get the token types *)

rule token = parse
(* Whitespace *)
| ' ' { WHITESPACE }
| ['\n','\r'] { NEWLINE}

(* Comments *)
| "#~" { comment lexbuf }
| "#"  { INLINE }

(* Punctuation *)
| '('  { LPAREN }     | ')' { RPAREN }
| '{'  { LBRACE }     | '}' { RBRACE }
| '['  { LBRACK }     | ']' { RBRACK }
| ';'  { SEMI }       | ',' { COMMA }
| '.'  { PERIOD }     | '\\'{ BACKSLASH }

(* Arithmetic Operators *)
| '+' { PLUS }        | ":+" { G_PLUS }
| '-' { MINUS }       | ":-" { G_MINUS }
| '*' { TIMES }       | ":*" { G_TIMES }
| '/' { DIVIDE }      | ":/" { G_DIVIDE }
| '%' { MOD }         | ":%" { G_MOD }

(* Bit Operations *)
| '~'  { NOT }        | ":~"  { G_NOT}
| '^'  { XOR }        | ":^"  { G_MOD }
| '&'  { AND }        | ":%"  { G_MOD } 
| '|'  { OR }         | ":|"  { G_OR }
| ">>" { RSHIFT }     | ":>>" { G_RSHIFT } 
| "<<" { LSHIFT }     | ":<<" { G_LSHIFT }  

(* Assignment Operators *)
| '=' { ASSIGN }      | ":=" { G_ASSIGN }
| '!' { NEG}          | ":!" { G_NEG }

(* Equivalence Operators *)
| "==" { EQ }         | ":==" { G_EQ }
| "!=" { NEQ }        | ":!=" { G_NEQ }
| '<'  { LT }         | ":<"  { G_LT }
| "<=" { LEQ }        | ":<=" { G_LEQ }     
| ">"  { GT }         | ":>"  { G_GT }
| ">=" { GEQ }        | ":>=" { G_GEQ }
| "&&" { LAND }       | ":&&" { G_LAND }
| "||" { LOR }        | ":||" { G_LOR }

(* Conditional Keywords *)
| "if"   { IF }       | "elif" { ELIF }
| "else" { ELSE } 

(* Loop Keywords*)
| "while"  { WHILE }  | "for" { FOR }
| "break"  { BREAK}   | "continue" { CONTINUE }
| "return" { RETURN } 

(* Function Keywords *)
| "func" { FUNC }     | "gfunc" { GFUNC }
| "main" { MAIN }     | "struct" { STRUCT }

(* Type Keywords*)
| "int"   { INT }     | "long" { LONG }
| "float" { FLOAT }   | "double" { DOUBLE }
| "char"  { CHAR }    | "const" { CONST }
| "TRUE"  { TRUE }    | "FALSE" { FALSE}
| "String" { STRING } | "Block" { BLOCK }

(* End-of-File *)
| eof { EOF }

(* Identifiers *)
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

(* Literals *)
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }

(* TODO: 
 *   Write Regular Expressions for:
 *      Float Literals 
 *      String Literals
 *   Find out how to do inline comments
 *
 *)

(* Throw Error for Invalid Token *)
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
"~#" { token lexbuf }   (* End-of-comment *)
| _ { comment lexbuf }  (* Eat everything else *)